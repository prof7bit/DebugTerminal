DebugTerminal
=============

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
