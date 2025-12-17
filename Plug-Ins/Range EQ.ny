;nyquist plug-in
;version 4
;type process
;name "Range EQ"
;author "Yaron Koresh"
;copyright "MIT"

;; --- הגדרת טווח ועוצמה ---
;control freq-start "Frequency A" float "Hz" 200 20 20000
;control freq-end "Frequency B" float "Hz" 4000 20 20000
;control total-gain "Total Gain (Peak/Depth)" float "dB" 0 -24 24

;; --- הגדרת מיקום השיא ---
;control peak-pos "Peak Position (Bias)" int "%" 50 5 95
;; 50% = השיא בדיוק באמצע הלוגריתמי
;; 5% = השיא קרוב מאוד להתחלה
;; 95% = השיא קרוב מאוד לסוף

;; --- עיצוב הצלעות ---
;control curve-left "Left Slope Shape" float "Curvature" 0 -10 10
;control curve-right "Right Slope Shape" float "Curvature" 0 -10 10
;; 0 = ישר
;; שלילי = בטן החוצה (כיפה)
;; חיובי = בטן פנימה (שפיץ)

;; --- פונקציות מתמטיות ---

(defun get-shaped-weight (pos curve-val)
  (cond
    ((= curve-val 0) pos)
    ((> curve-val 0) (power pos (+ 1.0 (/ curve-val 1.5)))) ; Concave
    ((< curve-val 0) (power pos (/ 1.0 (+ 1.0 (/ (abs curve-val) 1.5))))) ; Convex
  ))

(defun get-frequency-at-step (start end steps i)
  (let ((log-start (log start))
        (log-end (log end)))
    (exp (+ log-start (* i (/ (- log-end log-start) steps))))))

;; --- מנוע ה-EQ המשולב ---

(defun apply-ultimate-eq (sig f-start f-end max-g peak-pct c-left c-right)
  (let* (
         (log-start (log f-start))
         (log-end (log f-end))
         
         ;; חישוב מספר האוקטבות בטווח
         (total-octaves (/ (- log-end log-start) (log 2.0)))
         (safe-octaves (max (abs total-octaves) 0.1))
         
         ;; --- חישוב דינמי של כמות הבאנדים ---
         (bands-per-octave 4.0)
         (calc-bands (truncate (+ 0.5 (* safe-octaves bands-per-octave))))
         
         ;; הגבלות מינימום
         (bands (max 3 calc-bands))

         (processed-sig sig)
         
         ;; חישוב רוחב כל פרוסה לפי כמות הבאנדים
         (slice-width (/ safe-octaves bands))
         
         ;; פקטור למניעת בורות בין התדרים
         (final-width (* slice-width 1.65))

         (bias-factor (/ peak-pct 100.0))
         (log-center (+ log-start (* bias-factor (- log-end log-start))))
         (f-center (exp log-center))
        )
    
    ;; לולאת פילטרים
    (dotimes (i bands)
      (let* (
             (current-f (get-frequency-at-step f-start f-end (- bands 1) i))
             (weight 0.0)
            )
        ;; חישוב המשקל היחסי לכל תדר
        (cond
          ((< current-f f-center)
           (let* ((range-len (- log-center log-start))
                  (curr-pos (- (log current-f) log-start))
                  (rel-pos (if (> range-len 0) (/ curr-pos range-len) 0)))
             (setf weight (get-shaped-weight rel-pos c-left))))
          (t
           (let* ((range-len (- log-end log-center))
                  (curr-pos (- log-end (log current-f))) 
                  (rel-pos (if (> range-len 0) (/ curr-pos range-len) 0)))
             (setf weight (get-shaped-weight rel-pos c-right))))
        )
       
        ;; הפעלת הפילטר רק אם יש צורך
        (let ((gain-at-slice (* max-g weight)))
           (if (and (> (abs gain-at-slice) 0.01) ; סף רגישות
                    (< current-f (/ *sound-srate* 2.0)))
               (setf processed-sig (eq-band processed-sig current-f gain-at-slice final-width))))
      ))
 
    processed-sig))

;; --- הרצה ראשית ---

(if (arrayp *track*)
  (setf sr (snd-srate (aref *track* 0)))
  (setf sr (snd-srate *track*)))

(let ((nyquist-hz (/ sr 2.0)))
  ;; סידור ההתחלה והסוף
  (setf real-start (min freq-start freq-end))
  (setf real-end (max freq-start freq-end))
  
  ;; הגבלות בטיחות לתדרים
  (setf real-start (max 20.0 real-start))
  (setf real-end (min (- nyquist-hz 100.0) real-end))
  
  (if (< (- real-end real-start) 10.0)
      (setf real-end (+ real-start 10.0)))

  ;; הרצת האפקט
  (if (arrayp *track*)
      (vector
        (apply-ultimate-eq (aref *track* 0) real-start real-end total-gain peak-pos curve-left curve-right)
        (apply-ultimate-eq (aref *track* 1) real-start real-end total-gain peak-pos curve-left curve-right))
      (apply-ultimate-eq *track* real-start real-end total-gain peak-pos curve-left curve-right))
)