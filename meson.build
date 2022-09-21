project('s-lang',
        version: '0',
        default_options: ['warning_level=3'])

guile = find_program('guile', required: true, version: '>=3.0.5')
unsweetener = generator(find_program('./bootstrap/readable/unsweeten'),
                        capture: true,  # Capture output from stdout.
                        output: '@BASENAME@.scm',
                        arguments: ['@INPUT@'])

custom_target('s-lang',
              build_by_default: true,
              # List all files as input but only pass the entrypoint to the command.
              input: unsweetener.process('bootstrap/src/main.sscm',
                                         'bootstrap/src/tools.sscm',
                                         'bootstrap/src/parse/bindings.sscm',
                                         'bootstrap/src/parse/reading.sscm',
                                         preserve_path_from: meson.current_source_dir()),
              command: [guile, '--r7rs', '--no-auto-compile', '-s', '@INPUT0@'],
              output: 's-lang.wasm')
