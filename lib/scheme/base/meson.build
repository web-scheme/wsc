project('scheme-base',
        version: '0.1')

wat2wasm = generator(find_program('wat2wasm'),
                     arguments: ['--relocatable',
                                 '@INPUT@',
                                 '-o', '@OUTPUT@'],
                     output: '@BASENAME@.wasm')

wasm_ld = find_program('wasm-ld')

subdir('src')
