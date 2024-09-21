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

(defun PatchEnv (xf srate)
	(control-srate-abs srate
		(pwlv 0 xf 1 (- 0.5 xf) 1 0.5 0 1 0)
	)
)

(defun TrkEnv (xf srate)
	(control-srate-abs srate
		(pwlv 1 0.5 1 (+ 0.5 xf) 0 (- 1 xf) 0 1 1)
	)
)

(defun do-patch (g b)
	(setf srate (snd-srate g))
	(setf PEnv (PatchEnv 100 srate))
	(setf TEnv (TrkEnv 100 srate))
	(setf patch (mult (seq (cue g) (cue b)) PEnv))
	(setf patch (get-clip patch 0 (/ (get-sound-duration patch) 2)))
	(setf ret (sim
		(mult (seq (cue g) (cue b)) TEnv)
		(at-abs (get-sound-duration patch) (cue patch))
	))
	(setf half_dur (/ (get-sound-duration ret) 2.0))
	(get-clip ret half_dur half_dur)
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

(defun get-samples (sig)
  (setf srate (snd-srate sig))
  (setf max_len (roundup (* srate (get-duration 1))))
  (setf smps (snd-samples sig max_len))
smps)

;; ##############################################################################

(defun get-samples-snd (smps srate)
  (snd-from-array 0 srate smps)
)

;; ##############################################################################

(defun get-sample-snd (smp srate)
  (setf arr (make-array 1))
  (setf (aref arr 0) smp)
  (snd-from-array 0 srate arr)
)

;; ##############################################################################

(defun get-sample-amp (smp)
  (peak (get-sample-snd smp 48000) 1)
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

			(defun	;; name of function

;; FIT THE NEXT LINE(S) TO YOUR NEEDS!
remove-quiet

			(	;; parameters

;; FIT THE NEXT LINE(S) TO YOUR NEEDS!
sig

			)(let*(	;; variables

;; FIT THE NEXT LINE(S) TO YOUR NEEDS!
( x 0 )
( amp nil )
( rt3 nil )
( srate (snd-srate sig) )
( smps (get-samples sig) )
( ln (length smps) )

			)(do()(	;; until condition

;; FIT THE NEXT LINE(S) TO YOUR NEEDS!
(= x ln)

				;; return value

;; FIT THE NEXT LINE(S) TO YOUR NEEDS!
rt3

			)	;; operations for each loop

;; FIT THE NEXT LINE(S) TO YOUR NEEDS!
(setf amp (get-sample-amp (aref smps x) srate))
(if (not rt3)
  (setf rt3 (get-sample-snd (aref smps x) srate))
  (setf rt3 (_merge rt3 (get-sample-snd (aref smps x) srate)))
)
(setf x (+ 1 x))

			)))	;; end of function

;; ##############################################################################

			(defun	;; name of function

;; FIT THE NEXT LINE(S) TO YOUR NEEDS!
get-average-peaks

			(	;; parameters

;; FIT THE NEXT LINE(S) TO YOUR NEEDS!
monoin
chunk

			)(let*(	;; variables

;; FIT THE NEXT LINE(S) TO YOUR NEEDS!
( srate (snd-srate monoin) )
( sound_length (/ (snd-length monoin (truncate (* srate (get-duration 1)))) srate) )
( chunk_duration (min chunk sound_length) )
( samples_per_peak_test (truncate (* chunk_duration srate)) )
( chunk_duration (/ samples_per_peak_test srate) )
( clipped_len (truncate (* chunk_duration srate)) )
( trk_len (truncate (* (get-duration 1) srate)) )
( loops_count (roundup (/ (snd-length monoin trk_len) clipped_len)) )
( clp2 nil )
( lns 0 )
( result 0 )
( z 0 )

			)(do()(	;; until condition

;; FIT THE NEXT LINE(S) TO YOUR NEEDS!
(= z loops_count) 

				;; return value

;; FIT THE NEXT LINE(S) TO YOUR NEEDS!
(if (> result 0)
  (linear-to-db (/ result lns))
  0
)

			)	;; operations for each loop

;; FIT THE NEXT LINE(S) TO YOUR NEEDS!
(setf clp2 (get-clip monoin (* chunk_duration z) chunk_duration))
(setf pk (peak clp2 clipped_len))
(setf z (+ 1 z))
(setf pk (peak clp2 samples_per_peak_test))
(setf lns (+ lns 1))
(setf result (+ result pk))

			)))	;; end of function

;; ##############################################################################

(defun _merge (a b)
	(if a
		(if b
			(seq (cue a) (cue b))
			a
		)
		(if b
			b
			nil
		)
	)
)

;; ##############################################################################

(defun merge (snd_arr)
  (setf _rett (aref snd_arr 0))
  (do (
    (b 1 (1+ b))
  )( (= b (length snd_arr)))
    (setf _rett (_merge _rett (aref snd_arr b)))
  )
_rett)

;; ##############################################################################

(defun roundup (n)
  (if (= (truncate n) n)
    (truncate n)
    (+ (truncate n) 1)
  )
)

;; ##############################################################################

(defun new-sound ( &optional (du (get-duration 1)) (srate (snd-srate (aref *track* 0))) )
	(auto-scale (get-clip (resample (noise du) srate) 0 du) -180)
)

(defun new-silence ( &optional (du (get-duration 1)) (srate (snd-srate (aref *track* 0))) )
	(get-clip (resample (s-rest du) srate) 0 du)
)

;; ##############################################################################

(defun merge-at (sig tim2 sig2)
	(sim
		(at-abs 0 (cue sig))
		(at-abs tim2 (cue sig2))
	)
)

;; ##############################################################################

(defun connect-patch (sig1 sig2)

	(setf len1 (get-sound-duration sig1))
	(setf len2 (get-sound-duration sig2))

	(setf srate (snd-srate sig1))

	(if (not (= (snd-srate sig2) srate))
		(setf sig2 (resample sig2 srate))
	)

	(setf patching_duration 0.1)

	(setf sig1a (get-clip sig1 0 (- len1 patching_duration)))
	(setf sig1b (get-clip sig1 (- len1 patching_duration) patching_duration))

	(setf sig2a (get-clip sig2 0 patching_duration))
	(setf sig2b (get-clip sig2 patching_duration (* 2 patching_duration)))
	(setf sig2c (get-clip sig2 (* 3 patching_duration) (- len2 (* 3 patching_duration))))

	(setf s1 (merge-at
		(new-sound (+ len1 len2) srate)
		0
		sig1a
	))

	(setf s2 (merge-at
		(new-sound (* 2 patching_duration) srate)
		0
		sig1b
	))

	(setf s2 (merge-at
		s2
		patching_duration
		sig2a
	))

	(setf s2 (merge-at
		s1
		(- len1 patching_duration)
		(do-patch 
			sig2b
			s2
		)
	))

	(setf s3 (merge-at
		s2
		(+ len1 patching_duration)
		sig2b
	))

	(merge-at
		s3
		(+ len1 (* 3 patching_duration))
		sig2c
	)
)

;; ##############################################################################

(defun noise-gate (s freq db)

	(setf MODE "Gate")
	(setf STEREO-LINK "DoNotLink")
	(setf THRESHOLD (float db))
	(setf GATE-FREQ (float freq))
	(setf LEVEL-REDUCTION -100.0)
	(setf ATTACK 1.0)
	(setf HOLD 0.0)
	(setf DECAY 3000.0)

	(setf SILENCE-FLAG (if (> LEVEL-REDUCTION -96) 0 1))
	(setf FLOOR (db-to-linear LEVEL-REDUCTION))
	(setf THRESHOLD (db-to-linear THRESHOLD))
	(setf ATTACK (/ ATTACK 1000.0))
	(setf LOOKAHEAD ATTACK)
	(setf DECAY (/ DECAY 1000.0))
	(setf HOLD (/ HOLD 1000.0))

	(defun error-check ()
	  (let ((max-hz (* *sound-srate* 0.45))  ;10% below Nyquist should be safe maximum.
	        (max-khz (roundn (* 0.00045 *sound-srate*) 1))
	        (gate-freq-khz (roundn (/ GATE-FREQ 1000.0) 1)))
	    (when (>= GATE-FREQ max-hz)
	      (throw 'err (format nil
	                          (_ "Error.~%~
	                             Gate frequencies above: ~s kHz~%~
	                             is too high for selected track.~%~
	                             Set the control below ~a kHz.")
	                          gate-freq-khz
	                          max-khz))))
	  (when (< len 100) ;100 samples required 
	    (throw 'err (format nil
	                        (_ "Error.~%~
	                            Insufficient audio selected.~%~
	                            Make the selection longer than ~a ms.")
	                        (round-up (/ 100000 *sound-srate*))))))

	(defun analyze (sig)
	  ; Return analysis text.
	  (let* ((test-length (truncate (min len (/ *sound-srate* 2.0))))
	         (peakdb (peak-db sig test-length))
	         (target (+ 1.0 peakdb))) ;suggest 1 dB above noise level
	    (format nil
	            (_ "Peak based on first ~a seconds ~a dB~%~
	               Suggested Threshold Setting ~a dB.")
	            (roundn (/ test-length *sound-srate*) 2)
	            (roundn peakdb 2)
	            (roundn target 0))))

	(defun peak-db (sig test-len)
	  ;; Return absolute peak (dB).
	  ;; For stereo tracks, return the maximum of the channels.
	  (if (arrayp sig)
	      (let ((peakL (peak (aref sig 0) test-len))
	            (peakR (peak (aref sig 1) test-len)))
	        (linear-to-db (max peakL peakR)))
	      (linear-to-db (peak sig test-len))))

	(defun round-up (num)
	  (round (+ num 0.5)))

	(defun roundn (num places)
	  ;; Return number rounded to specified decimal places.
	  (if (= places 0)
	      (round num)
	      (let* ((x (format NIL "~a" places))
	             (ff (strcat "%#1." x "f")))
	        (setq *float-format* ff)
	        (format NIL "~a" num))))

	(defun format-time (s)
	  ;;; format time in seconds as h m.
	  (let* ((hh (truncate (/ s 3600)))
	         (mm (truncate (/ s 60))))
	  ;i18n-hint: hours and minutes. Do not translate "~a".
	  (format nil (_ "~ah ~am") hh (- mm (* hh 60)))))

	(defun noisegate (sig follow)
	  ;; Takes a sound and a 'follow' sound as arguments.
	  ;; Returns the gated audio.
	  (let ((gain (/ (- 1 (* SILENCE-FLAG FLOOR)))) ; SILENCE-FLAG is 0 or 1.
	        (env (get-env follow)))
	    (if (> GATE-FREQ 20)
	        (let* ((high (highpass8 sig GATE-FREQ))
	               (low  (lowpass8 sig (* 0.91 GATE-FREQ)))) ;magic number 0.91 improves crossover.
	          (sim (mult high gain env) low))
	        (mult sig gain env))))

	(defun get-env (follow)
	  ;; Return gate's envelope
	  (let* ((gate-env (gate follow LOOKAHEAD ATTACK DECAY FLOOR THRESHOLD))
	         (gate-env (clip gate-env 1.0)))  ;gain must not exceed unity.
	    (diff gate-env (* SILENCE-FLAG FLOOR))))

	(defun peak-follower (sig)
	  ;; Return signal that gate will follow.
	  (setf sig (multichan-expand #'snd-abs sig))
	  (when (and (arrayp sig)(= STEREO-LINK 0))
	    (setf sig (s-max (aref sig 0) (aref sig 1))))
	  (if (> HOLD 0)
	      (multichan-expand #'snd-oneshot sig THRESHOLD HOLD)
	      sig))

	(multichan-expand #' noisegate s (peak-follower s))
)

;; ##############################################################################

(defun limit (sig &optional (limit -0.1) (hold 1.0))
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

(defun vibrato (sig)
  (setf srate (snd-srate sig))
  (setf initial-depth 0.1)
  (setf mid-depth 0.1)
  (setf final-depth 0.1)
  (setf mid-pos 0.5)
  (setf initial-speed 7.0)
  (setf final-speed 7.0)
  (setf vib-depth (pwlv initial-depth mid-pos mid-depth 1 final-depth))
  (setf mod (hzosc (pwlv initial-speed 1 final-speed)))
  (setf p1 (/ (log 2.0) 6.0))
  (setf hz-ratio (s-exp (mult (mult vib-depth mod) p1)))
  (setf map (integrate hz-ratio))
  (snd-compose sig (force-srate srate map))
)

(defun hp1 (x fc)
  (if (< fc 493) (hp x fc)
      (if (< fc 2375)
          (scale-db (- (log fc) 6.2) (hp x fc))
          (scale-db (- (* (log fc) 2) 13.97) (hp x fc)))))

(defun sidechain (sig fc)
  (setf srate (snd-srate sig))
  (highpass2 
    (hp1 
      (scale-db 10
        (eq-highshelf sig (/ srate 3.1) -4 1.0))
      (* fc 1.07))
    (* fc 1.0) 1.06))

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
    (auto-scale
      (vibrato sig)
    2)
  -0.1)
)

;; ##############################################################################

(multichan-expand #'enhance *track* )
