DebugTerminal
=============

About
-----

DebugTerminal is a simple but very useful tool to help with embedded development. 
Especially in devices that have a very limited user interface or none at all there
sometimes is the need to watch or plot the value of a variable or even send simple 
commands to the device to assist in debugging. For this an asynchronous serial
interface is quickly implemented on the device or already present and this software
here serves as the other end of that communication channel.

It was inspired by programs like Bray's Terminal or CuteCom which serve the same
purpose but my needs (and therefore my main focus) was on plotting received data,
sometimes more than one variable per frame, optionally 16 bit signed or unsigned
values and it needed to be fast. And with fast I mean fast enough to handle tens 
of kS/s and still be able to plot them in real time.

DebugTerminal can demultiplex and plot in real time up to 4 channels of either 8 
or 16 bit values, either signed or unsigned. A sync button is used to align the
incoming data stream with the configured channels because there is no protocol,
no message delimiters, just a raw stream of data bytes or words, click the sync
button if high and low byte seem to be flipped, it will be shifted one byte per 
every click. 16 bit data is expected to be sent in little endian format (least 
significant byte first).

The terminal tab contains 8 speed buttons which can be configured to quickly
send custom byte sequences with a simnple click of the mouse.

Hex input is case insensitive and whitespace is ignored. Received data can be
displayed as hex, binary or decimal.

![Screen Shot](/screenshots/DebugTerminal_01.png?raw=true "Screen shot")
![Screen Shot](/screenshots/DebugTerminal_02.png?raw=true "Screen shot")
![Screen Shot](/screenshots/DebugTerminal_03.png?raw=true "Screen shot")
![Screen Shot](/screenshots/DebugTerminal_04.png?raw=true "Screen shot")
![Screen Shot](/screenshots/DebugTerminal_05.png?raw=true "Screen shot")
![Screen Shot](/screenshots/DebugTerminal_06.png?raw=true "Screen shot")

Download
--------

Please see the release section https://github.com/prof7bit/DebugTerminal/releases
for latest builds for Windows and Linux. There exist 2 Linux builds, one using
GTK2 and one using Qt4, the GTK build will run out of the box on any Linux 
distribution, the Qt-build will need Qt4Pas installed (available through your
distribution repositories). 

Windows binaries also will run out of the box and not need any dependencies.


Build it yourself
-----------------

You will need Lazarus and FPC. Linux users shold use fpcup (google it) to install
it and NOT use the distribution packages (which will probably be horribly outdated
or sometimes even outright broken). Alternatively you can download the .deb or .rpm
packages from sourceforge and follow the installation instructions.

Start Lazarus and open the project file (project -> open project -> DebugTerminal.lpi) 
and then from the menu choose: run -> build which will give you the executable file
a few seconds later.

If the project inspector is not visible then open it (project -> project inspector) 
to see an outline of the project and open source files for editing. 

Please also consult the the Lazarus forums if you want to dive deeper into the usage 
and possibilities of this powerful development environment.
