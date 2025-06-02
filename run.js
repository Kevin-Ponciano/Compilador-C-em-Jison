const fs = require('fs');
const parser = require('./compilador.js'); // nome do arquivo gerado

// Lê o arquivo .c passado como argumento na linha de comando
const filename = process.argv[2];
if (!filename) {
  console.error('Uso: node run.js arquivo.c');
  process.exit(1);
}

const code = fs.readFileSync(filename, 'utf8');

try {
  parser.parse(code);
} catch (e) {
  console.error('❌ Erro de compilação:', e.message);
}
