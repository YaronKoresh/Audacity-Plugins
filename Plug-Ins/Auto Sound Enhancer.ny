$nyquist plug-in
$version 4
$type process
$preview false
$name (_ "Auto Sound Enhancer")
$author (_ "Yaron Koresh")
$codetype lisp
$debugbutton false

;; ##############################################################################

(defun limit-memory (gb)
  (SND-SET-MAX-AUDIO-MEM (* 1024 1024 1024 gb))
) (limit-memory 16)

;; ##############################################################################

(defun auto-scale (trk limit)
  (setf pk (peak trk (+ (truncate (* (snd-srate trk) (get-duration 1))) 1)))
  (if (> pk 0)
    (if (not (= limit pk))
      (scale (/ (db-to-linear limit) pk) trk)
      trk
    )
    trk
  )
)

;; ##############################################################################

(defun limit (sig &optional (limit -0.1) (hold 0.1))
  (defun get-env (sig step lookahead limit)
    (let* ((sig (mult (/ limit) sig))
           (pad-time (* 3 lookahead))
           (pad-s (* 3 step))
           (padding (snd-const (peak sig pad-s) 0 (snd-srate sig) pad-time))
           (peak-env (snd-avg sig (* 4 step) step OP-PEAK)))
      (extract 0 1
          (s-max 1 
                 (sim padding
                      (at-abs pad-time (cue peak-env)))))))
  (let* ((time (/ hold 3000.0))
         (samples (round (* time (snd-srate sig))))  ; lookahead in samples
         (peak-env (get-env sig samples time (db-to-linear limit))))
    (mult sig
          (snd-exp (mult -1 (snd-log peak-env))))))

;; ##############################################################################

(defun roundup (n)
  (if (= (truncate n) n)
    (truncate n)
    (+ (truncate n) 1)
  )
)

;; ##############################################################################

(defun get-sound-length (sig)
  (snd-length sig (roundup (* (snd-srate sig) (get-duration 1))))
)

;; ##############################################################################

(defun get-sound-duration (sig)
  (/ (get-sound-length sig) (snd-srate sig))
)

;; ##############################################################################

(defun get-clip (sig &optional (t0 0) (tn (get-duration 1)))
  (if (= tn 0)
    nil
    (progn
      (setf sound_length (get-sound-duration sig))
      (if (> t0 sound_length)
        nil
        (extract-abs t0 (min (+ t0 tn) sound_length) (cue sig))
      )
    )
  )
)

;; ##############################################################################

(defun enhance (sig)
  (setf start (get '*selection* 'start))
  (setf end (get '*selection* 'end))
  (setf sig (get-clip sig start (- end start)))
  (setf limit 0.20)
  (setf lim1 (* -1 limit))
  (setf ratio1 (/ lim1))
  (setf lim2 (* lim1 -1.1))
  (setf ratio2 (/ lim2))
  (setf top (mult lim1 (s-max sig 0)))
  (setf bottom (sum (mult lim2 (s-min sig 0))))
  (setf sig (sim
    (scale lim1 (sum (s-exp top) -1))
    (scale lim2 (sum (s-exp bottom) -1))
  ))
  (limit
    (auto-scale sig 2)
  -0.1)
)

;; ##############################################################################

(multichan-expand #'enhance *track* )
