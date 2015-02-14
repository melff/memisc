## Copied from lattice, because R CMD --check warns about lattice:::paste.and.draw

### Copyright (C) 2001-2006 Deepayan Sarkar
### <Deepayan.Sarkar@R-project.org>
###
### This file is part of the lattice package for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA 

paste.and.draw <-
    function(left, right, sep = " : ",
             horizontal = TRUE,
             center = TRUE,
             showl = TRUE,
             showr = TRUE,
             gp = gpar())
{
    ## We are already in a viewport.  Essentially want to draw
    ## paste(left, right, sep = sep) in the middle.  The catch is,
    ## left and right (maybe even sep) may be expressions.  The easy
    ## solution is to draw sep in the middle and left and right on
    ## either side.  The better solution is to combine and then
    ## center.

    if (showl || showr)
    {
        shows <- showl && showr
        wsep <- unit(0.5 * shows, "strwidth", list(sep))
        offset <- unit(0.5, "npc")
        if (center)
            offset <-
                offset +
                    (if (showl) unit(0.5, "strwidth", list(left)) else unit(0, "mm")) -
                        (if (showr) unit(0.5 * showr, "strwidth", list(right)) else unit(0, "mm"))
        if (horizontal)
        {
            if (shows) grid.text(sep, x = offset,
                                 name = trellis.grobname("sep", type="strip"),
                                 gp = gp)
            if (showl) grid.text(left, x = offset - wsep,
                                 name = trellis.grobname("textl", type="strip"),
                                 gp = gp, just = "right")
            if (showr) grid.text(right, x = offset + wsep,
                                 name = trellis.grobname("textr", type="strip"),
                                 gp = gp, just = "left")
        }
        else
        {
            if (shows) grid.text(sep, y = offset,
                                 name = trellis.grobname("sep", type="strip.left"),
                                 gp = gp, rot = 90)
            if (showl) grid.text(left, y = offset - wsep,
                                 name = trellis.grobname("textl", type="strip.left"),
                                 gp = gp, just = "right", rot = 90)
            if (showr) grid.text(right, y = offset + wsep,
                                 name = trellis.grobname("textr", type="strip.left"),
                                 gp = gp, just = "left", rot = 90)
        }
    }
}
