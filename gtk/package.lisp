;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; package.lisp --- Package definition for gtk-cffi
;;;
;;; Copyright (C) 2007, Roman Klochkov <kalimehtar@mail.ru>
;;;

(in-package #:cl-user)

(defpackage gtk-cffi
  (:use #:common-lisp #:alexandria #:iterate
        #:cffi-objects #:g-object-cffi #:g-lib-cffi #:gdk-cffi 
        #:gtk-cffi-utils)
  (:shadow #:image #:window #:switch) ; gdk
  (:shadow #:maximize) ; iterate
  (:export
   ;;;; common
   #:gtk-init
   #:gtk-main
   #:gtk-main-quit
   #:gtk-model
   #:defmodel  ; recommended way

   #:foreach

   ;; reexport
   #:object-by-id
   #:gsignal
   #:yield

   #:css-provider
   #:css-provider-load

   #:widget-path
   #:to-string
   #:append-type
   #:append-for-widget
   #:prepend-type

   #:orientable
   #:orientation

   #:buildable

   #:adjustment
   #:value
   #:lower
   #:upper 
   #:step-increment
   #:page-increment
   #:page-size
   #:clamp-page
   #:changed
   #:value-changed
   #:minimum-increment

   #:style-context
   #:add-provider
   #:load-css
   #:junction-sides
   
   #:widget
   ;; widget slots
   #:name
   #:direction
   #:default-direction
   #:parent-window
   #:has-tooltip
   #:can-focus
   #:double-buffered
   #:events
   #:visual
   #:composite-name
   #:halign
   #:valign
   #:margin-left
   #:margin-right
   #:margin-top
   #:margin-bottom
   #:hexpand
   #:hexpand-set
   #:vexpand
   #:vexpand-set
   #:app-paintable
   #:size-request
   #:direction
   #:default-direction
   #:color
   #:font
   #:parent-window
   #:state
   #:bg-pixmap
   #:allocation
   #:parent
   #:child-visible
   #:tooltip-markup
   #:tooltip-text
   #:tooltip-window
   #:can-default
   #:has-window
   #:visible
   #:receives-default
   #:mapped
   #:realized
   #:no-show-all
   #:colormap
   #:sensitive
   #:widget-accel-path
   #:style-context
   #:device-events
   #:device-enabled
   #:toplevel
   #:ancestor
   #:is-ancestor
   #:path
   #:is-composited
   #:pango-context
   #:redraw-on-allocate ; setter only
   #:accessible
   #:settings
   #:clipboard
   #:display
   #:root-window
   #:screen
   #:has-screen
   #:allocated-width
   #:allocated-height
   #:is-sensitive
   #:is-focus
   #:state-flags
   #:has-default
   #:has-focus
   #:has-grab
   #:is-drawable
   #:is-toplevel
   #:device-is-shadowed
   #:preferred-height
   #:preferred-width
   #:preferred-size
   #:request-mode
   #:support-multidevice
   ;; methods
   #:activate
   #:show
   #:hide
   #:draw
   #:queue-draw
   #:queue-resize
   #:size-allocate
   #:add-accelerator
   #:remove-accelerator
   #:list-accel-closures
   #:can-activate-accel
   #:widget-event
   #:send-expose
   #:send-focus-change
   #:intersect
   #:grab-focus
   #:grab-default
   #:override-color
   #:override-background-color
   #:override-symbolic-color
   #:override-font
   #:override-cursor
   #:render-icon-pixbuf
   #:add-events
   #:get-pointer
   #:translate-coordinates
   #:shape-combine-region
   #:input-shape-combine-region
   #:create-pango-context
   #:create-pango-layout
   #:mnemonic-activate
   #:widget-map
   #:unmap
   #:realize
   #:unrealize
   #:child-focus
   #:child-notify
   #:freeze-child-notify
   #:thaw-child-notify
   #:destroy
   #:list-mnemonic-labels
   #:add-mnemonic-label
   #:remove-mnemonic-label
   #:error-bell
   #:keynav-failed
   #:trigger-tooltip-query
   #:reset-style
   #:queue-compute-expand
   #:compute-expand
   #:add-device-events
   #:reparent
   #:unparent
 
   #:pop-composite-child
   #:push-composite-child
   #:cairo-should-draw-window
   #:cairo-transform-to-window
   #:distribute-natural-allocation

   #:widget-class
   #:install-style-property
   #:install-style-property-parser
   #:list-style-properties
   #:find-style-property
   #:style-property

   #:invisible
   
   #:bin
   ;; methods
   #:child
   
   #:container
   ;; container slots
   #:border-width
   #:child-property
   #:focus-child
   #:focus-vadjustment
   #:resize-mode
   ;; methods
   #:add
   #:propagate-draw
   
   #:accel-group
   ;; methods
   #:connect
   #:disconnect

   #:window
   ;; window slots
   #:default-size
   #:screen
   #:transient-for
   #:window-position
   #:title
   #:role
   #:resizable
   #:modal
   #:gravity
   #:destroy-with-parent
   #:focus
   #:decorated
   #:deletable
   #:mnemonic-modifier
   #:type-hint
   #:skip-taskbar-hint
   #:skip-pager-hint
   #:urgency-hint
   #:accept-focus
   #:focus-on-map
   #:startup-id
   #:default-icon-list
   #:default-icon-name
   #:icon
   #:icon-list
   #:icon-name
   #:group
   #:opacity
   #:mnemonics-visible
   #:focus-visible
   #:has-resize-grip
   #:application
   #:window-size
   #:has-group
   ;; methods
   #:position-type
   #:add-accel-group
   #:remove-accel-group
   #:activate-focus
   #:activate-default
   #:set-geometry-hints
   #:is-active
   #:has-toplevel-focus
   #:list-toplevels
   #:add-mnemonic
   #:remove-mnemonic
   #:mnemonic-activate
   #:activate-key
   #:propagate-key-event
   #:default-widget
   #:present
   #:present-with-time
   #:iconify
   #:deiconify
   #:stick
   #:unstick
   #:maximize
   #:unmaximize
   #:fullscreen
   #:unfullscreen
   #:keep-above
   #:keep-below
   #:begin-resize-drag
   #:begin-move-drag
   #:window-type
   #:parse-geometry
   #:reshow-with-initial-size
   #:auto-startup-notification
   #:resize-grip-is-visible

   #:assistant
   ;; slots
   #:current-page
   #:page-type
   #:page-title
   #:page-complete
   #:forward-page-func
   ;; methods
   #:n-pages
   #:nth-page
   #:prepend-page
   #:append-page
   #:insert-page
   #:remove-page
   #:add-action-widget
   #:remove-action-widget
   #:update-buttons-state
   #:commit
   #:next-page
   #:previous-page

   #:offscreen-window
   #:surface
   #:pixbuf

   #:window-group
   ;; methods
   #:add-window
   #:remove-window
   #:list-windows
   #:current-grab
   #:current-device-grab
   
   #:icon
   ;; slots
   #:state-wildcarded
   #:size-wildcarded
   #:direction-wildcarded
   
   #:dialog
   ;;methods
   #:run
   #:response
   #:add-button
   #:default-response
   #:add-action-widget
   #:response-sensitive
   #:response-for-widget
   #:action-area
   #:content-area
   #:alternative-button-order
   #:alternative-dialog-button-order
   #:widget-for-response

   #:about-dialog
   ;;slots
   #:program-name
   #:version
   #:copyright
   #:comments
   #:license
   #:license-type
   #:website
   #:website-label
   #:authors
   #:artists
   #:documenters
   #:translator-credits
   #:logo
   #:logo-icon-name 

   #:editable
   ;; slots
   #:chars
   #:editable-position
   #:is-editable

   #:entry-completion
   ;; slots
   #:text-column
   #:minimum-key-length
   #:inline-completion
   ;; methods
   #:insert-action-markup
   #:insert-action-text
   #:popup-single-match
   #:complete
   #:insert-prefix
   #:compute-prefix
   #:completion-prefix
   #:delete-action

   #:entry-buffer
   ;; slots
   #:entry-buffer-length
   #:bytes
   ;; methods
   #:emit-deleted-text

   #:entry
   ;; entry slots
   #:text
   #:visibility
   #:max-length
   #:entry-buffer
   #:activates-default
   #:has-frame
   #:inner-border
   #:width-chars
   #:alignment
   #:overwrite-mode
   #:completion
   #:cursor-hadjustment
   #:progress-fraction
   #:progress-pulse-step
   #:text-length
   #:placeholder-text
   #:focus-hadjustment
   #:inline-selection
   #:popup-completion
   #:popup-set-width
   #:invisible-char
   #:unset-invisible-char
   ;; methods
   #:icon-storage-type
   #:progress-pulse
   #:delete-text
   #:set-icon-drag-source
   #:layout-index-to-text-index
   #:text-index-to-layout-index
   #:current-icon-drag-source
   #:icon-at-pos
  
   #:button
   ;; slots
   #:relief
   #:use-stock
   #:image-position
   ;; methods
   #:clicked
   #:event-window

   #:toggle-button
   #:inconsistent
   #:toggled

   #:check-button

   #:link-button
   #:uri
   #:visited

   #:radio-button
   #:group
   #:as-list
   #:join-group

   #:scale-button
   #:icons
   #:plus-button
   #:minus-button
   
   #:volume-button

   #:lock-button
   ; slot
   #:permission

   #:switch
   
   #:box
   ;; box slots
   #:spacing
   #:homogeneous
   ;; box methods
   #:pack
   #:pack*
   #:reorder-child
   #:child-packing

   #:v-box

   #:h-box

   #:button-box

   #:h-button-box

   #:event-box

   #:cell-renderer

   #:cell-renderer-text

   #:cell-renderer-toggle
   
   #:cell-renderer-pixbuf

   #:cell-layout
   #:reorder
   #:area

   #:cell-editable
   
   #:misc
   ;; misc slots
   #:alignment
   #:padding

   #:label
   ;; label slots
   #:text
   #:mnemonic-widget
   #:justify
   #:ellipsize
   #:width-chars
   #:max-width-chars
   #:line-wrap
   #:line-wrap-mode
   #:selectable
   #:attributes
   #:use-markup
   #:use-underline
   #:single-line-mode
   #:angle
   #:track-visited-links
   ;; methods
   #:pattern
   #:layout
   #:mnemonic-keyval
   #:select-region
   #:current-uri
   #:layout-offsets
   #:selection-bounds

   #:accel-label
   #:accel-widget
   #:accel-closure
   #:accel-width
   #:refetch

   #:with-markup

   #:paned
   #:h-paned
   #:v-paned
   #:paned-position

   #:frame
   ;; frame slots
   #:shadow-type

   #:tree-row-reference
   #:valid
   #:copy

   #:tree-path

   #:tree-model
   ;; tree-model slots
   #:columns
   ;; tree-model methods
   #:tree-model-foreach
   #:flags
   #:with-tree-iter
   #:n-columns
   #:column-type
   #:iter-has-child
   #:iter-n-children
   #:tree-iter
   #:iter->path
   #:iter->string
   #:path->iter
   #:iter-first
   #:iter-next
   #:iter-previous
   #:row-changed
   #:row-inserted
   #:row-deleted
   #:row-has-child-toggled
   #:rows-reordered
   #:ref-node
   #:unref-node
   
   
   #:list-store
   ;; list-store methods
   #:model-values
   #:append-values
   #:append-iter
   #:clear

   #:tree-model-filter
   ;; tree-model-filter slots
   #:visible-column
   #:with-child-path

   #:tree-view
   ;; tree-view slots
   #:model
   #:search-column
   #:expander-column
   #:level-indentation
   #:selection
   #:hover-expand
   #:rubber-banding
   #:headers-clickable
   #:show-expanders
   #:rules-hint
   #:headers-visible
   #:hover-selection
   #:search-equal-func #:tooltip-column
   #:search-entry
   #:row-sepearator-func
   #:fixed-height-mode
   #:search-position-func
   #:enable-tree-lines
   #:grid-lines 
   #:enable-search
   ;; tree-view methods
   #:is-rubber-banding-active
   #:create-row-drag-icon
   #:unset-rows-drag-source
   #:unset-rows-drag-dest
   #:bin-window
   #:append-column
   #:insert-column
   #:selection
   #:path-at-pos
   #:column
   #:cursor
   #:remove-column
   #:row-expanded #:expand-row
   #:expand-to-path #:collapse-row
   #:expand-all #:row-activated
   #:scroll-to-point #:collapse-all
   #:move-column-after
                           
   #:tree-view-column
   ;; slots
   #:sort-column-id
   #:alignment
   #:reorderable
   #:fixed-width
   #:max-width
   #:min-width
   #:expand
   #:sort-indicator
   #:sizing
   #:sort-order
   #:clickable
   ;; methods
   #:add-attribute
   #:cell-data-func
   #:cell-get-position
   #:cells
   #:get-cell-at
   #:clear-attributes
   #:x-offset
   #:cell-is-visible
   #:focus-cell
   #:cell-set-cell-data

   #:scrollable
   #:hscroll-policy
   #:vscroll-policy   
   
   #:scrolled-window
   ;; scrolled-window slots
   #:hadjustment
   #:vadjustment
   #:shadow-type
   #:placement
   #:min-content-width
   #:min-content-height
   #:policy
   ;; scrolled-window methods
   #:unset-placement
   #:add-with-viewport

   #:tree-selection
   ;; slots
   #:user-data
   #:mode
   #:select-function
   ;; methods
   #:select-path
   #:unselect-path
   #:select-iter
   #:unselect-iter
   #:select-all
   #:path-is-selected
   #:iter-is-selected
   #:unselect-range
   #:unselect-all
   #:count-selected-rows
   #:selected
   #:selected-rows

   #:text-mark
   ;; slots
   #:left-gravity
   #:deleted

   #:text-iter
   ;; slots
   #:line
   #:offset
   #:line-offset
   #:line-index
   #:visible-line-index
   #:visible-line-offset
   ;; methods
   #:text-iter-char
   #:slice
   #:text-iter-text
   #:visible-slice
   #:visible-text
   #:pixbuf
   #:marks
   #:toggled-tags
   #:child-anchor
   #:begins-tag
   #:ends-tag
   #:toggles-tag
   #:has-tag
   #:tags
   #:text-iter-editable
   #:can-insert
   #:starts-word
   #:ends-word
   #:inside-word
   #:starts-line
   #:starts-sentence
   #:ends-sentence
   #:inside-sentence
   #:is-cursor-position
   #:chars-in-line
   #:bytes-in-line
   #:get-attributes
   #:language
   #:is-end
   #:is-start
   #:forward-char
   #:backward-char
   #:forward-chars
   #:backward-chars
   #:forward-line
   #:backward-line
   #:forward-lines
   #:backward-lines
   #:forward-word-end
   #:backward-word-start
   #:forward-word-ends
   #:backward-word-starts
   #:forward-cursor-position
   #:backward-cursor-position
   #:forward-cursor-positions
   #:backward-cursor-positions
   #:backward-sentence-start
   #:forward-sentence-end
   #:backward-sentence-starts
   #:forward-sentence-ends
   #:forward-visible-word-end
   #:backward-visible-word-start
   #:forward-visible-word-ends
   #:backward-visible-word-starts
   #:forward-visible-cursor-position
   #:backward-visible-cursor-position
   #:forward-visible-cursor-positions
   #:backward-visible-cursor-positions
   #:forward-visible-line
   #:backward-visible-line
   #:forward-visible-lines
   #:backward-visible-lines
   #:forward-to-end
   #:forward-to-line-end
   #:forward-to-tag-toggle
   #:backward-to-tag-toggle
   #:forward-search
   #:backward-search
   #:text-iter-equal
   #:compare
   #:in-range
   #:order
   #:forward-find-char
   #:backward-find-char

   #:text-buffer
   ;; slot
   #:modified
   ;; methods
   #:line-count
   #:char-count
   #:tag-table
   #:insert-pixbuf
   #:insert-child-anchor
   #:create-child-anchor
   #:create-mark
   #:add-mark
   #:mark
   #:get-insert
   #:selection-bound
   #:has-selection
   #:place-cursor
   #:select-range
   #:remove-all-tags
   #:delete-selection
   #:paste-clipboard
   #:copy-clipboard
   #:cut-clipboard
   #:begin-user-action
   #:end-user-action
   #:add-selection-clipboard
   #:remove-selection-clipboard
   #:deserialize-can-create-tags
   #:copy-target-list
   #:paste-target-list
   #:register-deserialize-tagset
   #:register-serialize-tagset
   #:unregister-deserialize-format
   #:unregister-serialize-format
   #:start-iter
   #:end-iter 
   #:text
   #:insert
   #:insert-range
   #:text-buffer-delete
   #:backspace
   #:text-buffer-slice
   #:move-mark
   #:delete-mark
   #:apply-tag
   #:remove-tag
   #:create-tag
   #:text-buffer-iter
   #:bounds
   #:selection-bounds
   #:deserialize
   #:deserialize-formats
   #:serialize
   #:serialize-formats
   #:register-serialize-format
   #:register-deserialize-format
   #:lookup
   
   #:text-view
   ;; slots
   #:buffer
   #:wrap-mode
   #:editable
   #:cursor-visible
   #:overwrite
   #:pixels-above-lines
   #:pixels-below-lines
   #:pixels-inside-wrap
   #:justification
   #:left-margin
   #:right-margin
   #:indent
   #:tabs
   #:accepts-tab
   ;; methods
   #:scroll-to-mark
   #:scroll-to-iter
   #:scroll-mark-onscreen
   #:move-mark-onscreen
   #:place-cursor-onscreen
   #:text-view-window
   #:window-type
   #:border-window-size
   #:forward-display-line
   #:backward-display-line
   #:forward-display-line-end
   #:backward-display-line-start
   #:starts-display-line
   #:move-visually
   #:add-child-at-anchor
   #:add-child-in-window
   #:move-child
   #:default-attributes
   #:im-context-filter-keypress
   #:reset-im-context

   #:text-child-anchor
   #:widgets

   #:text-tag
   #:priority
   #:event
   #:ref
   #:unref

   #:text-appearance

   #:text-tag-table
   #:text-tag-table-remove
   
   #:text-attributes

   #:appearance
   #:direction
   #:text-attributes-font
   #:font-scale
   #:language
   #:invisible
   #:bg-full-height
   #:editable
   #:bg-color
   #:fg-color
   #:rise
   #:underline
   #:strikethrough
   #:draw-bg
   #:inside-selection
   #:is-text

   #:combo-box
   ;; slots
   #:wrap-width
   #:row-span-column
   #:column-span-column
   #:active
   #:active-iter
   #:id-column
   #:add-tearoffs
   #:title
   #:focus-on-click
   #:button-sensitivity
   #:entry-text-column
   #:popup-fixed-width
   ;; methods
   #:model
   #:active-id
   #:popup-for-device
   #:popup
   #:popdown
   #:row-separator-func
   #:has-entry
   #:active-id
   #:row-separator-func

   #:combo-box-text
   ;; methods
   #:combo-box-insert
   #:combo-box-prepend
   #:combo-box-append
   #:insert-text
   #:append-text
   #:prepend-text
   #:active-text
   #:combo-box-remove
   #:remove-all

   #:range
   ;; slots
   #:fill-level
   #:restrict-to-fill-level
   #:show-fill-level
   #:adjustment
   #:inverted
   #:value
   #:round-digits
   #:lower-stepper-sensitivity
   #:upper-stepper-sensitivity
   #:flippable
   #:min-slider-size
   #:slider-size-fixed
   ;; methods
   #:increments
   #:range
   #:slider-range
   #:range-rect

   #:append-text
   #:prepend-text
   #:insert-text
   #:remove-text
   #:active-text

   #:message-dialog
   #:markup
   #:image
   #:message-area
   #:format-secondary-text
   #:format-secondary-markup

   ;; handy defun
   #:show-message 

   #:file-chooser
   ;; file-chooser slots
   #:filename
   #:action
   #:local-only
   #:select-multiple
   #:show-hidden
   #:do-overwrite-confirmation
   #:create-folders
   #:current-folder-uri
   #:preview-widget
   #:preview-widget-active
   #:use-preview-label
   #:extra-widget
   #:filter

   #:file-filter
   ;; file-filter methods
   #:add-mime-type
   #:add-pattern
   #:add-pixbuf-formats
   #:needed
   
   #:file-chooser-dialog

   #:file-chooser-button
   
   #:progress-bar
   ;; progress-bar slots
   #:fraction
   #:inverted
   #:show-text
   #:ellipsize
   #:pulse-step
   ;; methods
   #:pulse

   #:table
   ;; table methods
   #:attach
   #:resize

   #:menu-shell

   #:menu

   #:menu-bar
   #:pack-direction
   #:child-pack-direction

   #:menu-item
   ;; slots
   #:right-justified
   #:reserve-indicator
   #:use-underline
   #:submenu
   #:accel-path
   ;; methods
   #:toggle-size-request
   #:toggle-size-allocate
   #:select
   #:deselect

   #:tool-shell

   #:toolbar

   #:notebook

   #:statusbar
   #:context-id
   #:statusbar-push
   #:statusbar-pop
   #:statusbar-remove
   #:message-area

   #:icon-source

   #:image
   ;; slots
   #:pixel-size
   #:animation

   #:expander
   ;; slots
   #:expanded
   #:label-fill
   #:label-widget

   #:application

   #:builder
   ;slot
   #:translation-domain
   ;methods
   #:add-from
   #:connect-dignals
   #:object
   #:objects
   #:type-from-name
   #:value-from-string

   #:status-icon
   #:size
   #:stock
   #:is-embedded
   #:gicon
   #:x11-window-id
   #:storage-type

   #:info-bar

   #:message-type
   

   #:spinner
   ;methods
   #:start
   #:stop

   #:activatable
   #:related-action
   #:use-action-appearance
   #:do-set-related-action
   #:sync-action-properties
   
   #:actionable
   #:action-name
   #:action-target-value
   #:detailed-action-name

   #:color-button
   #:rgba
   #:color
   #:alpha
   #:use-alpha
   #:title

   #:scale
   ;; slots
   #:digits
   #:value-pos
   #:draw-value
   #:has-origin
   ;; methods
   #:clear-marks

   #:spin-button
   ;; slots
   #:numeric
   #:update-policy
   #:wrap
   #:snap-to-ticks
   ;; methods
   #:value-as-int
   #:update
   #:spin
   ))

(in-package #:gtk-cffi)
(register-package "Gtk" *package*)
(register-prefix *package* 'gtk)