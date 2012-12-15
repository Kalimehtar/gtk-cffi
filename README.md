gtk-cffi
========

GTK-CFFI is a library, providing CFFI layer to GTK3. License is LLGPL for GTK, BSD for GLib and GDK.

GTK interface is mapped like this:

<table>
<tr><td>GTK</td><td>Lisp</td></tr>
<tr><td>gtk_widget_set_parent</td><td>(setf (parent widget) new-parent)</td></tr>
<tr><td>gtk_widget_get_parent</td><td>(parent widget)</td></tr>
</table>

Properties realized as (property object :property-name). There are corresponding setters for them. 
Signals: (gsignal object :signal-name). Value of signal can be name of C function, its address, 
corresponding keyword or lisp function, including closure.

Along with GtkListStore, I made LispStore. It can be filled much faster, than ListStore.

Why not cl-gtk2
===============
- cl-gtk2 supports only GTK2, gtk-cffi supports GTK3
- cl-gtk2 describes properties by hand, gtk-cffi uses g-object-class-find-property and caching results
- cl-gtk2 offers c-style functions like (gtk:widget-queue-resize-no-redraw widget), in gtk-cffi this will be (gtk:queue-resize widget :no-redraw t)

