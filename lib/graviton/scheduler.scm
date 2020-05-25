;;;
;;; scheduler.scm - Scheduler
;;;
;;;   Copyright (c) 2020 KOGURO, Naoki (naoki@koguro.net)
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

(define-module graviton.scheduler
  (use data.queue)
  (use gauche.threads)
  (use srfi-19)
  (use util.match)

  (export run-scheduler
          shutdown-scheduler!
          add-schedule!
          add-timeout!
          cancel-schedule!))

(select-module graviton.scheduler)

(define *scheduler-command-queue* (make-mtqueue))

(define (run-scheduler)
  (thread-start! (make-thread
                   (lambda ()
                     (guard (e (else (report-error e)
                                     (exit 70)))
                       (let loop ((schedule-list '()))
                         (let1 now (current-time)
                           (cond
                             ((and (not (null? schedule-list))
                                   (time<=? (caar schedule-list) now))
                              (match-let1 (_ thunk) (car schedule-list)
                                (thunk))
                              (loop (cdr schedule-list)))
                             (else
                              (let1 timeout (if (null? schedule-list)
                                                #f
                                                (max (- (time->seconds (caar schedule-list))
                                                        (time->seconds now))
                                                     0))
                                (match (dequeue/wait! *scheduler-command-queue* timeout #f)
                                  (('shutdown)
                                   #f)
                                  (('schedule wake-time thunk)
                                   (loop (sort (cons (list wake-time thunk) schedule-list)
                                               time<?
                                               car)))
                                  (('cancel thunk)
                                   (loop (remove (match-lambda
                                                   ((wake-time scheduled-thunk)
                                                    (eq? scheduled-thunk thunk)))
                                                 schedule-list)))
                                  (_
                                   (loop schedule-list))))))))))
                   "scheduler")))

(define (shutdown-scheduler!)
  (enqueue! *scheduler-command-queue* '(shutdown)))

(define (add-schedule! wake-time thunk)
  (enqueue! *scheduler-command-queue* (list 'schedule wake-time thunk)))

(define (make-time-from-second type sec)
  (let* ((sec-part (floor sec))
         (nanosec-part (round->exact (* (- sec sec-part)
                                        1000000000))))
    (make-time type nanosec-part sec-part)))

(define (add-timeout! timeout-in-sec thunk)
  (add-schedule!
      (add-duration (current-time)
                    (make-time-from-second time-duration timeout-in-sec))
    thunk))

(define (cancel-schedule! thunk)
  (enqueue! *scheduler-command-queue* (list 'cancel thunk)))
