This directory contains examples for Grafor library, as documented in book
"Grafor. Graphical extension for Fortran" by Bayakovsky, Mihaylova and Galaktionov.

The book is [available online](https://drive.google.com/file/d/1v7Qs9z1qghgrimA4e5mxXOEwisa0gcl-/view).

To enable graphics output, a card "*call plotter" must be present.
Three types of plotter are supported: Watanabe WX4675, Tektronix and Calcomp.
Please use one of these cards:

 * `*call plotter:wx4675,direct`
 * `*call plotter:tektronix,direct`
 * `*call plotter:calcomp,direct`

Watanabe is recommended, as it's the most advanced plotter.

# SVG output

The simulator automatically converts the plotter data to a picture
in [standard SVG format](https://en.wikipedia.org/wiki/SVG).
Files of this format can be easily viewed by any Web browser.

File name depends on selected plotter:

 * `watanabe.svg`
 * `tektronix.svg`
 * `calcomp.svg`

# Raw output

Bytes sent to the selected plotter are saved to a file with ".out" extension.
File name depends on selected plotter:

 * `watanabe.out`
 * `tektronix.out`
 * `calcomp.out`

# Multiple pictures

Some Grafor programs generate many pages of pictures.
For that, routines PAGE/ENDPG are called more than once.
In this case simulator creates several output files,
each file having a page number. For example:

 * `watanabe1.out`
 * `watanabe1.svg`
 * `watanabe2.out`
 * `watanabe2.svg`
 * `watanabe3.out`
 * `watanabe3.svg`

# Colors

Watanabe plotter has six changeable pens, numbered from 1 to 6.
The pens have the following colors:

 * pen #1 - black
 * pen #2 - red
 * pen #3 - green
 * pen #4 - blue
 * pen #5 - yellow
 * pen #6 - cyan

Use routine `getpen()` to change color, for example:

    call getpen(3)

# Example from the book

Here is a full example from the book, as shown on page 15 as Figure 1.3.
You can find it in file `demo.dub`.

    *name графор
    *call plotter:wx4675,direct
    *ftn
            program grafor
            real x(100), y(100), z(100)
            x(1) = -1.9
            y(1) = sin(x(1))
            z(1) = cos(x(1)) * 1.5
            do 5 i = 2, 90
                x(i) = x(i-1) + 0.1
                y(i) = sin(x(i))
                z(i) = cos(x(i)) * 1.5
      5         continue
            call getpen(2)
            call page(15., 20., 'PAGE', 4, 1)
            call getpen(3)
            call limits(-2., 7., -1.5, 1.5)
            call region(3., 3., 10., 15., 'REGION', 6, 1)
            call getpen(6)
            call axes('X axis', 6, 1., 5, 'Y axis', 6, 0.3, 4, 0)
            call getpen(5)
            call linemo(x, y, 85, 2, 10)
            call broken(0.5, 0.2, 0.3, 0.2)
            call brline(x, z, 85)
            call endpg(0)
            end
    *execute
    *end file

To run this example, use command:

    dubna demo.dub

See result in file `watanabe.svg`.
