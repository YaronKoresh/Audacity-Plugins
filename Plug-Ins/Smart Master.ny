;nyquist plug-in
;version 4
;type process
;name "Smart Master"
;author "Yaron Koresh"
;copyright "MIT"

;; --- ממשק: הגדרות כלליות ---
;control num-bands "Multiband: Count" int "bands" 5 1 64

;; --- ממשק: הגדרות באנד ראשון (Low) ---
;control th-start "Start: Threshold" real "dB" -20.0 -90.0 0.0
;control rat-start "Start: Ratio" real "x:1" 2.5 0.1 10.0
;control att-start "Start: Attack" real "ms" 15.0 0.1 100.0
;control rel-start "Start: Release" real "ms" 100.0 1.0 1000.0

;; --- ממשק: הגדרות באנד אחרון (High) ---
;control th-end "End: Threshold" real "dB" -20.0 -90.0 0.0
;control rat-end "End: Ratio" real "x:1" 1.5 0.1 10.0
;control att-end "End: Attack" real "ms" 5.0 0.1 100.0
;control rel-end "End: Release" real "ms" 50.0 1.0 1000.0

;; --- ממשק: שלב מאסטרינג סופי ---
;control target-rms "Master: Target Loudness" real "dB RMS" -12 -16 -4
;control character "Master: Character" choice "Neutral,Warm,Bright,Airy" 0
;control ceiling "Master: True Peak" real "dB" -0.5 -2.0 0.0

;; --- בדיקת קצב דגימה והגדרת בטיחות ---
(if (arrayp *track*)
  (setf sr (snd-srate (aref *track* 0)))
  (setf sr (snd-srate *track*)))

;; חישוב גבול תדר עליון (45% מקצב הדגימה)
(setf NYQ-LIMIT (* 0.45 sr))

;; הגדרת תדרים עם הגבלה אוטומטית למניעת קריסה
(setf MIN-FREQ 1.0)
(setf MAX-FREQ (min 22000.0 NYQ-LIMIT))
(setf LO-FREQ-EQ (min 160.0 (/ NYQ-LIMIT 4.0)))
(setf HI-FREQ-EQ (min 9000.0 NYQ-LIMIT))

;; הגדרת Lookahead בשניות
(setf LOOKAHEAD-MS 5.0)
(setf LOOKAHEAD-SEC (/ LOOKAHEAD-MS 1000.0))

;; --- מתמטיקה והמרות ---
(defun to-mono (sig)
  (if (arrayp sig)
      (scale 0.5 (sum (aref sig 0) (aref sig 1)))
      sig))

(defun roundup (number)
  (if (= (truncate number) number)
      (truncate number)
      (+ (truncate number) 1)))

(defun get-sound-length (sound)
  (snd-length (to-mono sound) (roundup (* sr (get-duration 1)))))

(defun val-db-to-lin (db) (power 10.0 (/ db 20.0)))

(defun val-lin-to-db (linear) 
  (if (> linear 0.00001) 
      (* 20.0 (/ (log linear) (log 10.0))) 
      -96.0))

(defun sound-lin-to-db (s)
  (scale 8.685889 (s-log (s-max 0.000001 s))))

(defun sound-db-to-lin (s)
  (s-exp (scale 0.115129 s)))

;; --- אינטרפולציה לינארית לפרמטרים ---
(defun lerp-param (start-val end-val current-idx total-bands)
  (if (<= total-bands 1)
      start-val
      (let ((frac (/ (float current-idx) (- total-bands 1.0))))
        (+ start-val (* frac (- end-val start-val))))))

;; --- מנוע קומפרסור / אקספנדר ---
(defun smart-dynamics (sig thresh rat att rel)
  (let* (
         (att-s (/ att 1000.0))
         
         ;; --- תיקון Lookahead ---
         ;; השהיה אמיתית של הסיגנל המקורי
         ;; (delay) מוסיף שקט בהתחלה, אבל זה נדרש ל-Lookahead
         (delayed-sig (seq (s-rest LOOKAHEAD-SEC) (cue sig)))
         
         ;; --- תיקון לייזר (RMS Limit) ---
         ;; חישוב קצב דגימת RMS. 
         ;; התיקון: אנו מגבילים את הקצב המקסימלי ל-100Hz (חלון של 10ms)
         ;; זה מונע מהקומפרסור "לרכב" על גלי הקול של הבאס ולעוות אותם
         (safe-rate (min 100.0 (/ 1000.0 (max 0.1 att))))

         (env (rms sig safe-rate))
         (env-db (sound-lin-to-db env))
         
         (diff-db (diff env-db thresh))
         (slope (if (> rat 1.0) (- 1.0 (/ 1.0 rat)) (- 1.0 rat)))
         
         (gain-db (scale (* -1.0 slope) (s-max 0.0 diff-db)))
         (gain-lin (sound-db-to-lin gain-db))
        )
    
    ;; אנו חותכים (Extract) את השקט שהוספנו בהתחלה
    ;; ומוודאים שהאורך חוזר להיות זהה למקור
    (mult delayed-sig gain-lin)))


;; --- ניהול באנדים רקורסיבי ---
(defun calc-log-crossovers (n min-f max-f)
  (if (< n 2) '() 
      (let ((freqs '()) (factor (power (/ max-f min-f) (/ 1.0 n))))
        (dotimes (i (- n 1)) (push (* min-f (power factor (+ i 1))) freqs))
        (reverse freqs))))

(defun recursive-band-process (sig crossovers current-band-idx)
  (let ((p-thresh (lerp-param th-start th-end current-band-idx num-bands))
        (p-ratio  (lerp-param rat-start rat-end current-band-idx num-bands))
        (p-attack (lerp-param att-start att-end current-band-idx num-bands))
        (p-release (lerp-param rel-start rel-end current-band-idx num-bands)))

    (if (null crossovers)
        (smart-dynamics sig p-thresh p-ratio p-attack p-release)
        (let ((cutoff (first crossovers)))
          (sum
            (smart-dynamics (lowpass8 sig cutoff) p-thresh p-ratio p-attack p-release)
            (recursive-band-process (highpass8 sig cutoff) (rest crossovers) (+ current-band-idx 1))
          )
        )
    )))

(defun stage-dynamic-multiband (sig)
  (let ((crossovers (calc-log-crossovers num-bands MIN-FREQ MAX-FREQ)))
    (recursive-band-process sig crossovers 0)))

;; --- מנוע ספקטרלי ---
(defun split-bands-eq (sig)
  (list (lowpass8 sig LO-FREQ-EQ)
        (lowpass8 (highpass8 sig LO-FREQ-EQ) HI-FREQ-EQ)
        (highpass8 sig HI-FREQ-EQ)))

(defun get-avg-rms (s &optional (precision 100))
  (let*
    (
      (rms-vals (rms (to-mono s) precision 2))
      (rms-len (get-sound-length rms-vals))
      (rms-val 0.0)
      (rms-avg 0)
    )
    (dotimes (i rms-len)
      (progn
        (setf rms-val (snd-fetch rms-vals))
        (setf rms-avg (sum rms-avg
          (* rms-val rms-val)
        ))
      )
    )
    (sqrt (/ rms-avg rms-len))
  )
)

(defun rms-scale (sound target-db)
  (let* ((target-linear (db-to-linear target-db))
         (source-linear (get-avg-rms sound)))
    (if (> source-linear 0.000001)
        (scale (/ target-linear source-linear) sound)
        sound)))

(defun get-spectral-profile (char-mode)
  (cond ((= char-mode 0) (list 0.0 -3.0 -6.0)) 
        ((= char-mode 1) (list 2.0 -3.0 -9.0)) 
        ((= char-mode 2) (list -1.0 -2.0 -4.0))
        ((= char-mode 3) (list -2.0 -3.0 -2.0))))

(defun stage-smart-eq (sig)
  (let* ((bands (split-bands-eq sig))
         (lo (first bands)) (mid (second bands)) (hi (third bands))
         (db-lo (get-avg-rms lo)) (db-mid (get-avg-rms mid)) (db-hi (get-avg-rms hi))
         (prof (get-spectral-profile character))
         (t-lo (first prof)) (t-mid (second prof)) (t-hi (third prof))
         (d-lo (- t-lo (- db-lo db-mid))) (d-hi (- t-hi (- db-hi db-mid))))
    (sum (scale (val-db-to-lin d-lo) lo) mid (scale (val-db-to-lin d-hi) hi))))

;; --- לימיטר ומנרמל ---
(defun stage-limiter (sig)
  (let* ((current-rms (get-avg-rms sig))
         (boosted (rms-scale sig target-rms))
         (limit (val-db-to-lin ceiling)))
    (s-min limit (s-max (* -1.0 limit) boosted))))

;; --- הרצה ---
(defun process-channel (sig)
  (let*
    (
      (original-dur (get-duration 1))
      (padded-sig (seq
        (cue (extract LOOKAHEAD-SEC (+ LOOKAHEAD-SEC original-dur) sig))
        (cue (s-rest LOOKAHEAD-SEC))
      ))
      (processed-shifted (stage-limiter
          (stage-smart-eq (stage-dynamic-multiband padded-sig))
      ))
    )
    processed-shifted
  )
)

(if (arrayp *track*)
    (vector (process-channel (aref *track* 0))
            (process-channel (aref *track* 1)))
    (process-channel *track*))