Tiny BASIC computer implemented in Clash
========================================

As seen in <https://unsafePerform.IO/blog/2020-11-17-a_tiny_computer_for_tiny_basic/>

This code is part of the book *Retrocomputing with Clash: Haskell for
FPGA Hardware Design* at <https://unsafePerform.IO/retroclash/>.

## Building into a bitfile

The included `mk` script runs the included Shake rules and creates a
bitfile ready to upload on a supported FPGA dev board. Targets are
made up from three specifiers: **core**, **IO** and **FPGA board**.

Available CPU cores:

- `intel8080`

Available IO modes:

- `serial`: serial IO
- `video`: PS/2 keyboard input, 640⨯480 VGA output

Available FPGA boards:

- [`papilio-one`][1] (with the Arcade MegaWing for `video`)
- [`papilio-pro`][2] (with the Arcade MegaWing for `video`)
- [`nexys-a7-50t`][3]

Note that only the Nexys A7-50T target is tested extensively, the
others might accumulate some bit-rot.

First, create a `build.mk` file that describes your local build
environment and your build target:

```
VIVADO_ROOT=/path/to/vivado/installation
TARGET=intel8080/video/nexys-a7-50t
```

Alternatively, if you are using the Vivado or ISE toolchain via a
wrapper script (e.g. to run it in Docker), instead of `VIVADO_ROOT` or
`ISE_ROOT`, you can set `VIVADO` or `ISE` to the wrapper script's name:

```
VIVADO=/path/to/vivado-wrapper
```

The script will be called with the first argument being the Vivado
tool's name, and the rest of the arguments are the arguments to the
tool itself.

Once you have `build.mk`, you can run `mk` and upload to your FPGA
board the `TinyBASICVIdeo.bit` file from the
`_build/intel8080/video/nexys-a7-50t/synth/TinyBASICVIdeo/TinyBASICVideo.runs/impl_1`
directory (or just do `./mk intel8080/video/nexys-a7-50t/upload`).

## Building the simulators

There are two simulators included:

* A ["very high-level"][4] simulation that only uses the CPU
  implementation from Clash, and the rest is Haskell.
  
* A logic board simulation that simulates not just the CPU, but also
  the memory elements, including the memory address decoding.
  
To build the simulations, just do a `stack build`. 

[1]: https://papilio.cc/index.php?n=Papilio.PapilioPro
[2]: https://papilio.cc/index.php?n=Papilio.PapilioOne
[3]: https://reference.digilentinc.com/reference/programmable-logic/nexys-a7/start
[4]: https://gergo.erdi.hu/blog/2018-09-15-very_high-level_simulation_of_a_c_ash_cpu/
