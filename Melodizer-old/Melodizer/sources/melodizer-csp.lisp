(in-package :mldz)

;;;;;;;;;;;;
; VARIATOR ;
;;;;;;;;;;;;

; <input-chords> is a voice object with the chords on top of which the melody is played
; <input-melody> is the melody that we wish to vary
; <key> is the key of the tonality
; <mode> is the mode of the tonality
; <fraction> is the proportion of notes from the melody that must be the same
(defmethod variator (input-chords input-melody key mode fraction)
    (let ((sp (gil::new-space)); create the space
            pitch intervals dfs tstop sopts nidentical initial-melody)

        ; initialize the variables
        (setq pitch (gil::add-int-var-array sp (om::n-pulses input-melody) 0 127))
        (setq intervals (gil::add-int-var-array sp (- (length pitch) 1) -127 127))

        ; temporary, should be set by the user
        (setq nidentical (ceiling (* fraction (length pitch))))

        ; get the initial melody's notes
        (setf initial-melody
            (loop :for n :from 0 :below (length (om::chords input-melody)) :by 1 collect (first (om::lmidic (nth n (om::chords input-melody)))))
        )
        (setf initial-melody (to-midi initial-melody))

        ; connect pitches to intervals
        (loop :for j :from 0 :below (length intervals) :do ;for each interval
            (let (temp)
                (setq temp (gil::add-int-var-array sp 3 -12 108)); temporary variables to make it easier to apply the linear constraint

                (gil::g-rel sp (first temp) gil::IRT_EQ (nth j pitch)); temp[0] = pitch[j]
                (gil::g-rel sp (second temp) gil::IRT_EQ (nth (+ j 1) pitch)); temp[1] = pitch[j+1]
                (gil::g-rel sp (third temp) gil::IRT_EQ (nth j intervals)); temp[2] = intervals[j]

                (gil::g-linear sp '(-1 1 -1) temp gil::IRT_EQ 0); -pitch[j] + pitch[j+1] - intervals[j] = 0
            )
        )

        ; post the constraints

        (range-restriction sp pitch input-chords)

        (interval-between-adjacent-notes sp pitch intervals)

        (in-tonality sp pitch key mode)

        (note-on-chord sp pitch input-melody input-chords)

        (harmonic-interval-chord sp pitch input-melody input-chords)

        ; constraint to ensure that the number of notes that are identical is right
        (at-least-n sp pitch initial-melody nidentical)

        ; branching
        (gil::g-branch sp pitch gil::INT_VAR_DEGREE_MAX gil::INT_VAL_RND)
        (gil::g-branch sp intervals gil::INT_VAR_SIZE_MIN gil::INT_VAL_RND)

        ;time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (gil::time-stop-init tstop 500); initialize it (time is expressed in ms)

        ;search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        (gil::set-n-threads sopts 1); set the number of threads to be used during the search (default is 1, 0 means as many as available)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ; search engine
        (setq se (gil::search-engine sp (gil::opts sopts) gil::DFS))

        (print "Variator CSP constructed"); print in the listener
        ; return
        (list se pitch tstop sopts intervals)
    )

)

;;;;;;;;;;;;;;;;;
; MELODY-FINDER ;
;;;;;;;;;;;;;;;;;

; <input> is a voice object with the chords on top of which the melody will be played
; <rhythm> the rhythm of the melody to be found in the form of a voice object
; <optional-constraints> is a list of optional constraint names that have to be applied to the problem
; <global interval> is the global interval that the melody should cover if the mostly increasing/decreasing constraint is selected
; <key> is the key in which the melody is
; <mode> is the mode of the tonality (major, minor)
; This function creates the CSP by creating the space and the variables, posting the constraints and the branching, specifying
; the search options and creating the search engine. 
(defmethod melody-finder (input rhythm optional-constraints &optional (global-interval nil) (key 60.0) (mode "major") (min-pitch 0) (max-pitch 200))
    (let ((sp (gil::new-space)); create the space; 
        pitch intervals dfs tstop sopts)

        ;initialize the variables
        (setq pitch (gil::add-int-var-array sp (om::n-pulses rhythm) 0 127))
        ; this can be as large as possible given the domain of pitch, to keep all the restrictions in the constraint part.
        (setq intervals (gil::add-int-var-array sp (- (length pitch) 1) -127 127))

        ; connect pitches to intervals
        (loop :for j :from 0 :below (length intervals) :do ;for each interval
            (let (temp)
                (setq temp (gil::add-int-var-array sp 3 -12 108)); temporary variables
                
                (gil::g-rel sp (first temp) gil::IRT_EQ (nth j pitch)); temp[0] = pitch[j]
                (gil::g-rel sp (second temp) gil::IRT_EQ (nth (+ j 1) pitch)); temp[1] = pitch[j+1]
                (gil::g-rel sp (third temp) gil::IRT_EQ (nth j intervals)); temp[2] = intervals[j]

                (gil::g-linear sp '(-1 1 -1) temp gil::IRT_EQ 0); -pitch[j] + pitch[j+1] - intervals[j] = 0
            )
        )
        ; post the constraints

        ; mandatory constraints

        (range-restriction sp pitch input)

        (interval-between-adjacent-notes sp pitch intervals)

        (in-tonality sp pitch key mode)

        (note-on-chord sp pitch rhythm input)

        (harmonic-interval-chord sp pitch rhythm input)
        
        ; optional constraints
        (post-optional-constraints optional-constraints sp pitch intervals global-interval min-pitch max-pitch)

        ; branching
        (gil::g-branch sp pitch gil::INT_VAR_DEGREE_MAX gil::INT_VAL_RND)
        
        ; the branching on intervals depends on the optional constraints. If there are none, do default branching
        (cond
            ((or (find "strictly-increasing-pitch" optional-constraints :test #'equal) (find "increasing-pitch" optional-constraints :test #'equal) (find "mostly-increasing-pitch" optional-constraints :test #'equal))
                (gil::g-branch sp intervals gil::INT_VAR_SIZE_MIN gil::INT_VAL_SPLIT_MAX)
            )
            ((or (find "strictly-decreasing-pitch" optional-constraints :test #'equal) (find "decreasing-pitch" optional-constraints :test #'equal) (find "mostly-decreasing-pitch" optional-constraints :test #'equal))
                (gil::g-branch sp intervals gil::INT_VAR_SIZE_MIN gil::INT_VAL_SPLIT_MIN)
            )
            (T ; default behaviour 
                (gil::g-branch sp intervals gil::INT_VAR_SIZE_MIN gil::INT_VAL_RND)
            )
        )

        ;time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (gil::time-stop-init tstop 500); initialize it (time is expressed in ms)

        ;search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        (gil::set-n-threads sopts 1); set the number of threads to be used during the search (default is 1, 0 means as many as available)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ; search engine
        (setq se (gil::search-engine sp (gil::opts sopts) gil::DFS))

        (print "melody-finder CSP constructed")
        ; return
        (list se pitch tstop sopts intervals)
    )
)


;;;;;;;;;;;;;;;;;
; NEW-MELODIZER ;
;;;;;;;;;;;;;;;;;

; <input> is a voice object with the chords on top of which the melody will be played
; <rhythm> the rhythm of the melody to be found in the form of a voice object
; <optional-constraints> is a list of optional constraint names that have to be applied to the problem
; <global interval> is the global interval that the melody should cover if the mostly increasing/decreasing constraint is selected
; <key> is the key in which the melody is
; <mode> is the mode of the tonality (major, minor)
; This function creates the CSP by creating the space and the variables, posting the constraints and the branching, specifying
; the search options and creating the search engine. 
(defmethod new-melodizer ()
    (let ((sp (gil::new-space)); create the space;
        push pull playing dfs tstop sopts scaleset pitch
        (bars 4)
        (quant 8)
        (major-natural (list 2 2 1 2 2 2 1)))
        (setf scaleset (build-scaleset major-natural))
        

        ;initialize the variables
        (setq push (gil::add-set-var-array sp (* bars quant) 0 127))
        (setq pull (gil::add-set-var-array sp (* bars quant) 0 127))
        (setq playing (gil::add-set-var-array sp (* bars quant) 0 127))
         
        ;initial constraint on pull and playing
        ;(gil::g-rel sp (first pull) gil::IRT_EQ empty) ; pull[0] == empty
        (gil::g-rel sp (first push) gil::SRT_EQ (first playing)) ; push[0] == playing [0]

        ;connect push, pull and playing
        (loop :for j :from 1 :below (* bars quant) :do ;for each interval
            (let (temp temp2)
                (setq temp (gil::add-set-var-array sp 3 0 127)); temporary variables
                (setq temp2 (gil::add-set-var-array sp 2 0 127)); temporary variables
                 
                (gil::g-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) (first temp)); temp[0] = playing[j-1] - pull[j]
                (gil::g-op sp (nth j playing) gil::SOT_UNION (nth j push) (second temp)); playing[i] == playing[j-1] - pull[i] + push[i] Playing note
                 
                (gil::g-rel sp (nth j pull) gil::SRT_SUB (nth (- j 1) playing)) ; pull[i] <= playing[i-1] cannot pull a note not playing
                 
                (gil::g-set-op sp (nth (- j 1) playing) gil::SOT_UNION (nth j pull) gil::SRT_DISJ (nth j push)); push[j] || playing[j-1] + pull[j] Cannot push a note still playing
            )
        )

        (gil::g-card sp playing 0 10) ; piano can only 10 notes at a time
        (gil::g-card sp pull 0 10) ; can't release more notes than we play
        (gil::g-card sp push 0 5) ; can't start playing more than 5 notes at a time
         
        ; Following a scale
        (loop :for j :from 0 :below (* bars quant) :do
            (gil::g-rel sp (nth j push) gil::SRT_SUB scaleset)      
        )

        ; branching
        (gil::g-branch sp push nil nil) 
        (gil::g-branch sp pull nil nil) 

        ;time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (gil::time-stop-init tstop 500); initialize it (time is expressed in ms)

        ;search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        (gil::set-n-threads sopts 1); set the number of threads to be used during the search (default is 1, 0 means as many as available)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ; search engine
        (setq se (gil::search-engine sp (gil::opts sopts) gil::DFS))

        (print "new-melodizer CSP constructed")
        ; return
        (list se playing tstop sopts)
    )
)

;posts the optional constraints specified in the list
; TODO CHANGE LATER SO THE FUNCTION CAN BE CALLED FROM THE STRING IN THE LIST AND NOT WITH A SERIES OF IF STATEMENTS
(defun post-optional-constraints (optional-constraints sp notes intervals global-interval min-pitch max-pitch)
    (if (find "all-different-notes" optional-constraints :test #'equal)
        (all-different-notes sp notes)
    )
    (if (find "minimum-pitch" optional-constraints :test #'equal)
        (minimum-pitch sp notes min-pitch)
    )
    (if (find "maximum-pitch" optional-constraints :test #'equal)
        (maximum-pitch sp notes max-pitch)
    )
    (if (find "strictly-increasing-pitch" optional-constraints :test #'equal)
        (strictly-increasing-pitch sp notes)
    )
    (if (find "strictly-decreasing-pitch" optional-constraints :test #'equal)
        (strictly-decreasing-pitch sp notes)
    )
    (if (find "increasing-pitch" optional-constraints :test #'equal)
        (increasing-pitch sp notes)
    )
    (if (find "decreasing-pitch" optional-constraints :test #'equal)
        (decreasing-pitch sp notes)
    )
    (if (find "mostly-increasing-pitch" optional-constraints :test #'equal)
        (mostly-increasing-pitch sp notes intervals global-interval)
    )
    (if (find "mostly-decreasing-pitch" optional-constraints :test #'equal)
        (mostly-decreasing-pitch sp notes intervals global-interval)
    )
)

;;;;;;;;;;;;;;;
; SEARCH-NEXT ;
;;;;;;;;;;;;;;;

; <l> is a list containing the search engine for the problem and the variables
; <rhythm> is the input rhythm as given by the user 
; <melodizer-object> is a melodizer object
; this function finds the next solution of the CSP using the search engine given as an argument
(defmethod search-next (l rhythm melodizer-object)
    (let ((se (first l))
         (pitch* (second l))
         (tstop (third l))
         (sopts (fourth l))
         (intervals (fifth l))
         (check t); for the while loop
         sol pitches)

        (om::while check :do
            (gil::time-stop-reset tstop);reset the tstop timer before launching the search
            (setq sol (gil::search-next se)); search the next solution
            (if (null sol) 
                (stopped-or-ended (gil::stopped se) (stop-search melodizer-object) tstop); check if there are solutions left and if the user wishes to continue searching
                (setf check nil); we have found a solution so break the loop
            )
        )

        (setq pitches (to-midicent (gil::g-values sol pitch*))); store the values of the solution
        (print "solution found")

        ;return a voice object that is the solution we just found
        (make-instance 'voice
            :tree rhythm
            :chords pitches
            :tempo (om::tempo (input-rhythm melodizer-object))
        )
    )
)

; <l> is a list containing the search engine for the problem and the variables
; <rhythm> is the input rhythm as given by the user 
; <melodizer-object> is a melodizer object
; this function finds the next solution of the CSP using the search engine given as an argument
(defmethod new-search-next (l melodizer-object)
    (let ((se (first l))
         (playing (second l))
         (tstop (third l))
         (sopts (fourth l))
         (check t); for the while loop
         sol score)

        (om::while check :do
            (gil::time-stop-reset tstop);reset the tstop timer before launching the search
            (setq sol (gil::search-next se)); search the next solution
            (if (null sol) 
                (stopped-or-ended (gil::stopped se) (stop-search melodizer-object) tstop); check if there are solutions left and if the user wishes to continue searching
                (setf check nil); we have found a solution so break the loop
            )
        )

         ;créer score qui retourne la liste de pitch et la rhythm tree
        (print "avant score")
        (setq score (build-score sol playing)); store the values of the solution TODO to midicent
        (print "solution found")

        ;return a voice object that is the solution we just found
        (make-instance 'voice
            :tree (second score)
            :chords (first score)
            :tempo (om::tempo (input-rhythm melodizer-object))
        )
    )
)

; determines if the search has been stopped by the solver because there are no more solutions or if the user has stopped the search
(defun stopped-or-ended (stopped-se stop-user tstop)
    (if (= stopped-se 0); if the search has not been stopped by the TimeStop object, there is no more solutions
        (error "There are no more solutions.")
    )
    ;otherwise, check if the user wants to keep searching or not
    (if stop-user
        (error "The search has been stopped. Press next to continue the search.")
    )
)

