custom_target('s-lang',
              build_by_default: true,
              # List all files as input but only pass the entrypoint to the command.
              input: unsweetener.process('main.sscm',
                                         'parse/env.sscm',
                                         'parse/macros.sscm',
                                         'parse/reading.sscm',
                                         'tools.sscm',
                                         'tools/logging.sscm',
                                         preserve_path_from: meson.current_source_dir()),
              command: [guile, '--r7rs', '--no-auto-compile', '-s', '@INPUT0@'],
              output: 's-lang.wasm')
