# xscreensaver-dbus

xscreensaver-dbus implements the freedesktop.org D-Bus API for screensavers,
translating D-Bus calls into appropriate invocations of xscreensaver-command.
This allows D-Bus-compliant applications to interface with xscreensaver.

Currently, xscreensaver-dbus only supports the [Idle Inhibition
Service](https://people.freedesktop.org/~hadess/idle-inhibition-spec/)
interface, but adding additional interfaces should be straightforward.
