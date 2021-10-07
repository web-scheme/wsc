base = custom_target('base',
                     input: [wat2wasm.process('base.wat'),
                             wat2wasm.process('booleans.wat')],
                     output: 'base.wasm',
                     command: [wasm_ld,
                               '--no-entry',
                               '@INPUT@',
                               '-o', '@OUTPUT@'],
                     build_by_default: true)
