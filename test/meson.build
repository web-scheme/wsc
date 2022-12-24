test_reader = find_program('cat')

test('macros',
     test_reader,
     args: [custom_target('macros-test',
                          depends: [s_lang],
                          # List all files as input and pass them all to the command.
                          # The test runner sets up the environment and loads each test file.
                          input: unsweet.process('runner.sscm',
                                                 'tap.sscm',
                                                 'unit/macros.sscm',
                                                 preserve_path_from: meson.current_source_dir()),
                          command: [guile, '--r7rs', '--no-auto-compile', '@INPUT@'],
                          capture: true,  # Capture output from stdout.
                          output: 'macros.tap')],
     protocol: 'tap',
     suite: 'unit')

test('compilation',
     test_reader,
     args: [custom_target('compilation-test',
                          depends: [s_lang],
                          # List all files as input and pass them all to the command.
                          # The test runner sets up the environment and loads each test file.
                          input: unsweet.process('runner.sscm',
                                                 'tap.sscm',
                                                 'unit/compilation.sscm',
                                                 preserve_path_from: meson.current_source_dir()),
                          command: [guile, '--r7rs', '--no-auto-compile', '@INPUT@'],
                          capture: true,  # Capture output from stdout.
                          output: 'compilation.tap')],
     protocol: 'tap',
     suite: 'unit')
