project('WSC', version: '0')

guile = find_program('guile', required: true, version: '>=3.0.1')
unsweet = generator(find_program('./bootstrap/readable/unsweeten'),
                    capture: true,  # Capture output from stdout.
                    output: '@BASENAME@.scm',
                    arguments: ['@INPUT@'])

subdir('src')
subdir('test')
