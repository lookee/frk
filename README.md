# frk

`frk` is an experimental parallel Mandelbrot and Julia fractal set generator written as my first Erlang application.

## License

Copyright (C) 2011 Luca Amore `<luca.amore@gmail.com>`

`frk` is released under the GNU General Public License version 3 (or, at your option, any later version). See [COPYING](COPYING) for the full text or visit <https://www.gnu.org/licenses/>.

## Requirements

- Erlang (tested with release R13B03 on GNU/Linux Ubuntu 10.10)
- ImageMagick (tested with 6.6.2-6) for converting the generated PPM images to PNG or other formats

## Project Structure

```text
frk/
├── src/               # Erlang source code
├── ebin/              # compiled BEAM modules
├── save/              # output for generated images
├── frkMandelbrot.sh   # Mandelbrot shell script
├── frkJulia.sh        # Julia shell script
├── demo.sh            # sample script
├── Makefile / Emakefile
├── COPYING            # license: GPL v3+
└── README.md          # project overview
```

## Usage

### Provided Scripts

| Script             | Description                       |
|--------------------|-----------------------------------|
| `frkMandelbrot.sh` | Generates Mandelbrot fractal images |
| `frkJulia.sh`      | Generates Julia fractal images      |

Both scripts rely on ImageMagick for conversion from `.ppm` to `.png`. Run either script with `-h` to view the available options, or execute the script without arguments to generate the default image. All images are saved inside the `save/` directory.

## Contact

- Email: <luca.amore@gmail.com>
- Website: <https://www.lucaamore.com>

Happy hacking!  
Luca Amore

### Screen
