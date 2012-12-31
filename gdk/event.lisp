(in-package :gdk-cffi)

(defctype device :pointer)

(defctype window pobject)

(defctype axes :pointer) ;; array of double

(defcenum extension-mode
  :none :all :cursor)

(defbitfield event-mask
  :exposure
  :pointer-motion
  :pointer-motion-hint
  :button-motion
  :button1-motion
  :button2-motion
  :button3-motion
  :button-press
  :button-release
  :key-press
  :key-release
  :enter-notify
  :leave-notify
  :focus-change
  :structure
  :property-change
  :visibility-notify
  :proximity-in
  :proximity-out
  :substructure
  :scroll
  (:all #x3ffffe))

(defcenum event-type
  (:nothing -1)
  :delete :destroy :expose :motion-notify
  :button-press :button2-press :button3-press :button-release
  :key-press :key-release :enter-notify :leave-notify
  :focus-change :configure :map :unmap :property-notify
  :selection-clear :selection-request :selection-notify
  :proximity-in :proximity-out
  :drag-enter :drag-leave :drag-motion :drag-status 
  :drop-start :drop-finished :client-event :visibility-notify
  :no-expose :scroll :window-state :setting :owner-change 
  :grab-broken :damage)

(defcstruct event-any
  ""
  (type :int)
  (window window)
  (send-event :int8))

(defcstruct event-key
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (time :uint32)
  (state modifier-type)
  (keyval key)
  (length :int)
  (string :string)
  (hardware-keycode :uint16)
  (group :uint8)
  (is_modifier :uint))

(defcstruct event-button
  "GdkEventButton"
  (type event-type)
  (window window)
  (send-event :int8)
  (time :uint32)
  (x :double)
  (y :double)
  (axes axes)
  (state modifier-type)
  (button :int)
  (device device)
  (x-root :double)
  (y-root :double))

(defcenum scroll-direction
  :up :down :left :right)

(defcstruct event-scroll
  "GdkEventScroll"
  (type event-type)
  (window window)
  (send-event :int8)
  (time :uint32)
  (x :double)
  (y :double)
  (state modifier-type)
  (direction scroll-direction)
  (device device)
  (x-root :double)
  (y-root :double))

(defcstruct event-motion
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (time :uint32)
  (x :double)
  (y :double)
  (axes axes)
  (state modifier-type)
  (is-hint :int16)
  (device device)
  (x-root :double)
  (y-root :double))

(defcstruct rectangle
  ""
  (x :int) (y :int)
  (width :int) (height :int))

(defctype region :pointer) ;; = GdkRegion*

(defcstruct* event-expose
  (event-expose-type event-type)
  (window window)
  (send-event :int8)
  (area (:struct rectangle))
  (region region)
  (event-expose-count :int))

(defcenum visibility-state
  :unobscured :partial :obscured)

(defcstruct event-visibility
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (state visibility-state))

(defcenum crossing-mode
  :normal :grab :ungrab)

(defcenum notify
  :ancestor :virtual :inferior :nonlinear :nonlinear-virtual :unknown)

(defcstruct event-crossing
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (subwindow window)
  (time :uint32)
  (x :double)
  (y :double)
  (x-root :double)
  (y-root :double)
  (mode crossing-mode)
  (detail notify)
  (focus :boolean)
  (state modifier-type))

(defcstruct event-focus
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (in :int16))


(defcstruct event-configure
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcenum property-state
  :new-value :delete)

(defctype gdk-atom :pointer)

(defcstruct event-property
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (atom gdk-atom)
  (time :uint32)
  (state property-state))

(defctype native-window :uint32)

(defcstruct event-selection
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (selection gdk-atom)
  (target gdk-atom)
  (property gdk-atom)
  (time :uint32)
  (requestor native-window))

(defctype drag-context :pointer)

(defcstruct event-dnd
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (context drag-context)
  (time :uint32)
  (x-root :short) (y-root :short))

(defcstruct event-proximity
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (time :uint32)
  (device device))

(defcunion client-data-union
  (b :char :count 20)
  (s :short :count 10)
  (l :long :count 5))

(defcstruct event-client
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (message-tyoe gdk-atom)
  (data-format :ushort)
  (data client-data-union)) ; :union

(defcstruct event-no-expose
  ""
  (type event-type)
  (window window)
  (send-event :int8))

(defbitfield window-state
  :withdrawn :iconified :maximized :sticky :fullscreen :above :below)

(defcstruct event-window-state
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (changed-mask window-state)
  (new-window-state window-state))

(defcenum setting-action
  :new :changed :deleted)

(defcstruct event-setting
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (acion setting-action)
  (name :string))

(defcenum owner-change
  :new :destroy :close)

(defcstruct event-owner-change
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (owner native-window)
  (reason owner-change)
  (selection gdk-atom)
  (time :uint32)
  (selection-time :uint32))

(defcstruct event-grab-broken
  ""
  (type event-type)
  (window window)
  (send-event :int8)
  (keyboard :boolean)
  (implicit :boolean)
  (grab-window window))

(defcunion event
  (type event-type)
  (any event-any)
  (expose event-expose)
  (no-expose event-no-expose)
  (visibility event-visibility)
  (motion event-motion)
  (button event-button)
  (scroll event-scroll)
  (key event-key)
  (crossing event-crossing)
  (focus-change event-focus)
  (configure event-configure)
  (property event-property)
  (selection event-selection)
  (owner-change event-owner-change)
  (proximity event-proximity)
  (client event-client)
  (dnd event-dnd)
  (window-state event-window-state)
  (setting event-setting)
  (grab-broken event-grab-broken))

(defclass event (object)
  ((event-type :accessor event-type)))

(defmethod initialize-instance
  :after ((event event)
          &key pointer &allow-other-keys)
  (setf (event-type event)
        (case (foreign-slot-value pointer 'event 'type) ; :union
          ((:nothing :delete :destroy :map :unmap) 'event-any)
          (:expose 'event-expose)
          (:motion-notify 'event-motion)
          ((:button-press :button2-press
                          :button3-press :button-release) 'event-button)
          ((:key-press :key-release) 'event-key)
          ((:enter-notify :leave-notify) 'event-crossing)
          (:focus-change 'event-focus)
          (:configure 'event-configure)
          (:property-notify 'event-property)
          ((:selection-clear :selection-request
                             :selection-notify) 'event-selection)
          ((:proximity-in :proximity-out) 'event-proximity)
          ((:drag-enter :drag-leave :drag-motion
                        :drag-status :drop-start
                        :drop-finished) 'event-dnd)
          (:client-event 'event-client)
          (:visibility-notify 'event-visibility)
          (:no-expose 'event-no-expose)
          (:scroll 'event-scroll)
          (:window-state 'event-window-state)
          (:setting 'event-setting)
          (:owner-change 'event-owner-change)
          (:grab-broken 'event-grab-broken)
          (t 'event-any))))

(defmethod get-slot ((event event) field)
  (foreign-slot-value (pointer event) 
                      (cffi-objects::struct-type (event-type event))
                      (find-symbol (string field) :gdk-cffi)))

(defun parse-event (ev-pointer field)
  (get-slot (if (pointerp ev-pointer)
                (make-instance 'event :pointer ev-pointer)
                ev-pointer) field))