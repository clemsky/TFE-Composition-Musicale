(in-package :mldz)

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
(defmethod new-melodizer (block-csp)
    (let ((sp (gil::new-space)); create the space;
        push pull playing pushMap pullMap dfs tstop sopts scaleset pitch temp
        (max-pitch 127)
        (bars (bar-length block-csp))
        (quant 192)
        (min-length 1) ;minimum length of a note with associated constraint
        (chord-rhythm 2) ;a chord is played every [chord-rhythm] quant
        (chord-min-length 2) ; minimum length of a chord with associated constraint
        (major-natural (list 2 2 1 2 2 2 1)) ; represent intervals of the scale we are composing in
        (chord-prog (list 1 5 6 4))) ; represent the chord progression we want to follow
        ;(setf scaleset (build-scaleset major-natural))
        (setf chordset (build-chordset chord-prog major-natural))
        (setf progsize (length chord-prog))

        (print block-csp)
        (setq temp (get-sub-block-values sp block-csp))
        (setq push (nth 0 temp))
        (setq pull (nth 1 temp))
        (setq playing (nth 2 temp))
        (setq notes (nth 3 temp))
        (setq added-notes (nth 4 temp))

        ; chord rhythm
        ;(loop :for j :from 0 :below (* bars quant) :by 1 :do
        ;      (if (= (mod j chord-rhythm) 0)
        ;         (gil::g-card sp (nth j push) 3 3)
        ;          (gil::g-card sp (nth j push) 0 1)
        ;      )
        ;)

        ; chord length (need previous constraint to work)
        ;(loop :for j :from 0 :below (* bars quant) :by 1 :do
        ;      (if (= (mod j chord-rhythm) 0)
        ;          (loop :for k :from 1 :below chord-min-length :while (< (+ j k) (* bars quant)) :do
        ;              (gil::g-rel sp (nth (+ j k) pull) gil::SRT_DISJ (nth j push))
        ;          )
        ;      )
        ;)

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
        (list se push pull tstop sopts bars quant notes added-notes)
    )
)

(defun get-sub-block-values (sp block-csp)
    ; for block child of block-csp
    ; (pull supersets de get-sub-block-values(block) )
    ; constraints
    ; return pull push playing
    (let (pull push playing pushMap pullMap block-list positions max-notes
         (bars (bar-length block-csp))
         (quant 192)
         (major-natural (list 2 2 1 2 2 2 1))
         (max-pitch 127))
         ;(setf scaleset (build-scaleset major-natural))

         (setq max-notes (* 127 (* bars quant)))
         (print bars)
         (print quant)

        ;initialize the variables

        (setq push (gil::add-set-var-array sp (* bars quant) 0 max-pitch 0 max-pitch))
        (setq pull (gil::add-set-var-array sp (* bars quant) 0 max-pitch 0 max-pitch))
        (setq playing (gil::add-set-var-array sp (* bars quant) 0 max-pitch 0 max-pitch))

        ;channeling array with time as index to array with pitch as index
        (setq pushMap (gil::add-set-var-array sp (+ max-pitch 1) 0 (* bars quant) 0 (* bars quant)))
        (setq pullMap (gil::add-set-var-array sp (+ max-pitch 1) 0 (* bars quant) 0 (* bars quant)))
        (gil::g-channel sp push pushMap)
        (gil::g-channel sp pull pullMap)


        (setq block-list (block-list block-csp))
        (if (not (typep block-list 'list))
            (progn
            (print "in typep")
            (setq block-list (list block-list)))
        )
        (setq positions (position-list block-csp))

        ;initial constraint on pull, push and playing
        (gil::g-empty sp (first pull)) ; pull[0] == empty
        (gil::g-empty sp (car (last push)))  ; push[bars*quant] == empty
        (gil::g-rel sp (first push) gil::SRT_EQ (first playing)) ; push[0] == playing [0]



        ;compute notes
        (setq notes (gil::add-int-var sp 0 max-notes))
        (setq notes-array (gil::add-int-var-array sp (* bars quant) 0 127))
        (loop :for i :from 0 :below (* bars quant) :by 1 :do
            (gil::g-card-var sp (nth i push) (nth i notes-array))
        )
        (gil::g-sum sp notes notes-array)

        ;compute added notes
        (setq added-push (gil::add-set-var-array sp (* bars quant) 0 max-pitch 0 max-pitch))
        (setq added-notes (gil::add-int-var sp 0 127))
        (setq added-notes-array (gil::add-int-var-array sp (* bars quant) 0 127))
        (loop :for i :from 0 :below (* bars quant) :by 1 :do
            (gil::g-card-var sp (nth i added-push) (nth i added-notes-array))
        )
        (gil::g-sum sp added-notes added-notes-array)

        ;connect push, pull and playing
        (loop :for j :from 1 :below (* bars quant) :do ;for each interval
            (let (temp)
                (setq temp (gil::add-set-var sp 0 max-pitch 0 max-pitch)); temporary variables

                (gil::g-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) temp); temp[0] = playing[j-1] - pull[j]
                (gil::g-op sp temp gil::SOT_UNION (nth j push) (nth j playing)); playing[j] == playing[j-1] - pull[j] + push[j] Playing note

                (gil::g-rel sp (nth j pull) gil::SRT_SUB (nth (- j 1) playing)) ; pull[j] <= playing[j-1] cannot pull a note not playing

                (gil::g-set-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) gil::SRT_DISJ (nth j push)); push[j] || playing[j-1] - pull[j] Cannot push a note still playing
            )
        )

        (if (melody-source block-csp)
            (let (melody-temp melody-push melody-pull melody-playing)
                (setq melody-temp (create-push-pull (melody-source block-csp) quant))
                (setq melody-push (gil::add-set-var-array sp (length (first melody-temp)) 0 max-pitch 0 max-pitch))
                (setq melody-pull (gil::add-set-var-array sp (length (second melody-temp)) 0 max-pitch 0 max-pitch))
                (setq melody-playing (gil::add-set-var-array sp (length (third melody-temp)) 0 max-pitch 0 max-pitch))
                (loop :for i :from 0 :below (length (first melody-temp)) :by 1 :do
                    (if (or (typep (nth i (first melody-temp)) 'list) (/= (nth i (first melody-temp)) -1))
                        (gil::g-rel sp (nth i melody-push) gil::SRT_EQ (nth i (first melody-temp)))
                        (gil::g-empty sp (nth i push))
                    )
                )
                (loop :for i :from 0 :below (length (second melody-temp)) :by 1 :do
                    (if (or (typep (nth i (second melody-temp)) 'list) (/= (nth i (second melody-temp)) -1))
                        (gil::g-rel sp (nth i melody-pull) gil::SRT_EQ (nth i (second melody-temp)))
                        (gil::g-empty sp (nth i pull))
                    )
                )
                (loop :for i :from 0 :below (length (third melody-temp)) :by 1 :do
                    (if (or (typep (nth i (third melody-temp)) 'list) (/= (nth i (third melody-temp)) -1))
                        (gil::g-rel sp (nth i melody-playing) gil::SRT_EQ (nth i (third melody-temp)))
                        (gil::g-empty sp (nth i melody-playing))
                    )
                )
                (loop :for j :from 0 :below (length melody-push) :by 1 :do
                        (gil::g-rel sp (nth j melody-push) gil::SRT_SUB (nth j push))
                        (gil::g-rel sp (nth j melody-pull) gil::SRT_SUB (nth j pull))
                        ;(gil::g-rel sp (nth j melody-playing) gil::SRT_SUB (nth j playing))
                )
            )
        )


        (if (not (endp block-list))
            ; make the push and pull array supersets of the corresponding array of the child blocks
            (let ((sub-push-list (list))
                (sub-push (gil::add-set-var-array sp (* bars quant) 0 max-pitch 0 max-pitch)))

                (loop :for i :from 0 :below (* bars quant) :by 1 :do
                    (setq temp (gil::add-set-var-array sp (length block-list) 0 max-pitch 0 max-pitch))
                    (setq sub-push-list (nconc sub-push-list (list temp)))
                    (gil::g-setunion sp (nth i sub-push) (nth i sub-push-list))
                    (gil::g-op sp (nth i added-push) gil::SOT_DUNION (nth i push) (nth i sub-push))
                )
                (loop :for i :from 0 :below (length block-list) :by 1 :do
                      (let (tempPush tempPull tempPlaying tempList (start (* (nth i positions) quant)))
                           (setq tempList (get-sub-block-values sp (nth i block-list)))
                           (setq tempPush (first tempList))
                           (setq tempPull (second tempList))
                           (setq tempPlaying (third tempList))

                           (loop :for j :from start :below (+ start (length tempPlaying)) :by 1 :do
                                (gil::g-rel sp (nth (- j start) tempPush) gil::SRT_SUB (nth j push))
                                (gil::g-rel sp (nth (- j start) tempPull) gil::SRT_SUB (nth j pull))
                                (gil::g-rel sp (nth (- j start) tempPlaying) gil::SRT_SUB (nth j playing))

                                (gil::g-rel sp (nth (- j start) tempPush) gil::SRT_EQ (nth i (nth j sub-push-list)))
                           )
                      )
                )
            )
            ; if no block-list
            (gil::g-rel sp added-notes gil::SRT_EQ notes)
        )

        ;constraints
        (post-optional-constraints sp block-csp push pull playing pushMap notes added-notes)

        (pitch-range sp push (min-pitch block-csp) (max-pitch block-csp))
        (list push pull playing notes added-notes)
    )
)

;posts the optional constraints specified in the list
; TODO CHANGE LATER SO THE FUNCTION CAN BE CALLED FROM THE STRING IN THE LIST AND NOT WITH A SERIES OF IF STATEMENTS
(defun post-optional-constraints (sp block push pull playing pushMap notes added-notes)


    ; Block constraints
    (if (voices block)
      (gil::g-card sp playing 0 (voices block))
    )

    (if (min-added-notes block)
        (gil::g-rel sp added-notes gil::IRT_GQ (min-added-notes block))
    )

    (if (max-added-notes block)
        (gil::g-rel sp added-notes gil::IRT_LQ (max-added-notes block))
    )

    ; Time constraints
    (if (min-note-length block)
        (note-min-length sp push pull (min-note-length block))
    )
    (if (quantification block)
        (set-quantification sp push pull (quantification block))
    )

    ; Pitch constraints
    ; following a scale
    (if (key-selection block)
        (if (mode-selection block)
            (let ((bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                  (scale (get-scale (mode-selection block)))  ;if - mode selectionné
                  (offset (- (name-to-note-value (key-selection block)) 60)))
                 (setf scaleset (build-scaleset scale offset))
                 (print scaleset)
                 (gil::g-rel sp bool gil::SRT_EQ 1) ;forcer le reify a true dans ce cas
                 (scale-follow-reify sp push scaleset bool))
            (let ((bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                  (scale (get-scale "ionian (major)"))  ;else - pas de mode selectionné => major natural
                  (offset (- (name-to-note-value (key-selection block)) 60)))
                 (gil::g-rel sp bool gil::SRT_EQ 1) ;forcer le reify a true dans ce cas
                 (setf scaleset (build-scaleset scale offset))
                 (scale-follow-reify sp push scaleset bool))
        )
        (if (mode-selection block)
            (let ((bool-array (gil::add-bool-var-array sp 12 0 1))) ; créer le booleen pour la reify
                (loop :for key :from 0 :below 12 :by 1 :do
                    (setf scale (get-scale (mode-selection block)))
                    (setf scaleset (build-scaleset scale key))
                    (print scaleset)
                    (scale-follow-reify sp push scaleset (nth key bool-array))
                )
                (gil::g-rel sp gil::BOT_OR bool-array 1)
            )
        )
    )


    (if (chord-key block)
        (if (chord-quality block)
            (let ((bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                  (chord (get-chord (chord-quality block)))  ;if - mode selectionné
                  (offset (- (name-to-note-value (chord-key block)) 60)))
                 (setf chordset (build-scaleset chord offset))
                 (gil::g-rel sp bool gil::SRT_EQ 1) ;forcer le reify a true dans ce cas
                 (scale-follow-reify sp push chordset bool))
        )
        (if (chord-quality block)
            (let ((bool-array (gil::add-bool-var-array sp 12 0 1))) ; créer le booleen pour la reify
                (loop :for key :from 0 :below 12 :by 1 :do
                    (setf chord (get-chord (chord-quality block)))
                    (setf chordset (build-scaleset chord key))
                    (scale-follow-reify sp push chordset (nth key bool-array))
                )
                (gil::g-rel sp gil::BOT_OR bool-array 1)
            )
        )
    )


    (if (pitch-direction block)
        (let ((allPlayed (gil::add-set-var sp 0 (+ (length push) 1) 0 (+ (length push) 1)))
              (isPlayed (gil::add-bool-var-array sp (+ (length push) 1) 0 1)))
             (gil::g-arr-op sp gil::SOT_UNION pushMap allPlayed)
             (gil::g-channel sp isPlayed allPlayed)

            (cond
                ;((string= (pitch-direction block) "Moslty increasing")    (moslty-increasing-pitch sp))
                ((string= (pitch-direction block) "Increasing")           (increasing-pitch sp playing isPlayed))
                ;((string= (pitch-direction block) "Strictly increasing")  (strictly-increasig-pitch sp))
                ;((string= (pitch-direction block) "Moslty decreasing")    (mostly-decreasig-pitch sp))
                ((string= (pitch-direction block) "Decreasing")           (decreasing-pitch sp playing isPlayed))
                ;((string= (pitch-direction block) "Strictly Decreasing")  (strictly-decreasig-pitch sp))
            )
        )
    )

)

;;;;;;;;;;;;;;;
; SEARCH-NEXT ;
;;;;;;;;;;;;;;;

; <l> is a list containing the search engine for the problem and the variables
; <rhythm> is the input rhythm as given by the user
; <melodizer-object> is a melodizer object
; this function finds the next solution of the CSP using the search engine given as an argument
(defmethod new-search-next (l melodizer-object)
    (let ((se (first l))
         (push (second l))
         (pull (third l))
         (tstop (fourth l))
         (sopts (fifth l))
         (bars (sixth l))
         (quant (seventh l))
         (notes (eighth l))
         (added-notes (ninth l))
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

        ; (print (gil::g-values sol notes))
        ; (print (gil::g-values sol added-notes))
         ;créer score qui retourne la liste de pitch et la rhythm tree

        (setq score (build-score sol push pull bars quant (tempo melodizer-object))); store the values of the solution

        ;return a voice object that is the solution we just found
        (make-instance 'voice
            :tree (second score)
            :chords (first score)
            :tempo (tempo melodizer-object)
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
