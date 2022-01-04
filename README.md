This is my attempt to implement CIP (Constrained Interpolation Profile method) on a Cartesian cut cell grid.

Please note that this simulator is not fully functional and probably not worth building.  I only publish the code in case it is of interest to others.

I developed this simulator many years ago for my PhD.  Before I could complete it I started encountering bugs in the Intel Fortran compiler (and I couldn't switch to using the GNU Fortran compiler because it didn't fully support the Fortran 2003 features I wanted to use). I stopped work on the simulator and pivoted my PhD project to study the properties of the proposed CIP/cut cell scheme in the frequency domain.

It turns out that even if the simulator were fully functional, it probably wouldn't be numerically stable if it were used to simulate pure advection. I discovered and presented this in my thesis which you can find [here](https://era.ed.ac.uk/handle/1842/9698).
