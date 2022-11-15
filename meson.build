project('s-lang',
        version: '0',
        default_options: ['warning_level=3'])

guile = find_program('guile', required: true, version: '>=3.0.5')
unsweetener = generator(find_program('./bootstrap/readable/unsweeten'),
                        capture: true,  # Capture output from stdout.
                        output: '@BASENAME@.scm',
                        arguments: ['@INPUT@'])

subdir('src')
subdir('test')
