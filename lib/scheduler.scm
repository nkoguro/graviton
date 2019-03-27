;;;
;;; scheduler.scm - Scheduler
;;;
;;;   Copyright (c) 2019 KOGURO, Naoki (naoki@koguro.net)
;;;   All rights reserved.
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define *scheduler-thread* #f)
(define *scheduler-channel* #f)

(define (current-timeofday)
  (receive (sec usec) (sys-gettimeofday)
    (+ sec (/. usec 1000000))))

(define (run-scheduler)
  (let1 channel (make-mtqueue)
    (set! *scheduler-thread*
          (make-thread
            (lambda ()
              (guard (e (else (notify-exception e)))
                (define (handle-event event schedules)
                  (match event
                    ('exit
                      #f)
                    (#f
                     (next-schedule schedules))
                    ((sec . thunk)
                     (next-schedule (sort (acons sec thunk schedules)
                                          (lambda (s0 s1)
                                            (< (car s0) (car s1))))))))
                (define (next-schedule schedules)
                  (let1 now-sec (current-timeofday)
                    (match schedules
                      (()
                       (handle-event (dequeue/wait! channel) '()))
                      (((next-sec . thunk) rest ...)
                       (cond
                         ((<= next-sec now-sec)
                          (guard (e (else (notify-exception e)))
                            (thunk))
                          (next-schedule rest))
                         (else
                          (handle-event (dequeue/wait! channel (- next-sec now-sec)) schedules)))))))
                (next-schedule '())))))
    (set! *scheduler-channel* channel)
    (thread-start! *scheduler-thread*)))

(define (shutdown-scheduler)
  (when *scheduler-thread*
    (enqueue! *scheduler-channel* 'exit)
    (thread-join! *scheduler-thread*)
    (set! *scheduler-thread* #f)
    (set! *scheduler-channel* #f)))

(define (kill-scheduler)
  (when *scheduler-thread*
    (thread-terminate! *scheduler-thread*)
    (set! *scheduler-thread* #f)
    (set! *scheduler-channel* #f)))

(define (add-timer! sec thunk)
  (enqueue! *scheduler-channel* (cons (+ (current-timeofday) sec) thunk)))
