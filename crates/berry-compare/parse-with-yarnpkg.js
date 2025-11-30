#!/usr/bin/env node

import fs from 'fs';
import path from 'path';
import { parseSyml } from '@yarnpkg/parsers';

const filePath = process.argv[2];

if (!filePath) {
  console.error('Usage: parse-with-yarnpkg.js <path-to-lock-file>');
  process.exit(1);
}

try {
  const content = fs.readFileSync(filePath, 'utf-8');
  const parsed = parseSyml(content);
  
  // Output as JSON for Rust to parse
  console.log(JSON.stringify(parsed, null, 2));
} catch (error) {
  console.error(JSON.stringify({
    error: error.message,
    stack: error.stack
  }));
  process.exit(1);
}
