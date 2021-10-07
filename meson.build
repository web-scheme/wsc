project('S-lang',
        version: '0.1',
        default_options: ['warning_level=3'])

lexer_generator = generator(find_program( 'flex', required: true),
                            output: '@BASENAME@.yy.c',
                            arguments: ['-o', '@OUTPUT@', '@INPUT@'])
lexer = lexer_generator.process('lexer.l')

parser_generator = generator(find_program('bison', required: true),
                             output: ['@BASENAME@.tab.c', '@BASENAME@.tab.h'],
                             arguments: ['@INPUT@', '--defines=@OUTPUT1@', '--output=@OUTPUT0@'])
parser = parser_generator.process('parser.y')

executable('s-lang',
           lexer, parser,
           install: true)
