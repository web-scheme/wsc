# TODO: Use a proper test target: [https://github.com/mesonbuild/meson/issues/10915].
custom_target('unit',
              depends: [s_lang],
              # List all files as input and pass them all to the command.
              # The test runner sets up the environment and loads each file.
              input: unsweetener.process('runner.sscm',
                                         'tap.sscm',
                                         'unit/macros.sscm',
                                         preserve_path_from:
                                           meson.current_source_dir()),
              command: [guile, '--r7rs', '--no-auto-compile', '@INPUT@'],
              capture: true,  # Capture output from stdout.
              output: 'unit.tap')
