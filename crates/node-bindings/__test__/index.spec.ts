import test from 'ava'
import fs from 'node:fs'

import { parse, parseFile } from '../index'

test('can parse a file', (t) => {
	// cwd is actually not the test dir, but the root of the project
	// needs us to be buffered
	const fileContents = fs.readFileSync('../../fixtures/berry.lock')
	const entries = parse(fileContents)
	t.log(`parsed ${entries} entries`)
	t.true(entries > 0)
})

test('throws on a malformed lockfile', (t) => {
	t.throws(() => parse(Buffer.from('__metadata:\n  bogus\n')))
})

test('can parse a file by path', (t) => {
	t.true(parseFile('../../fixtures/berry.lock') > 0)
	t.throws(() => parseFile('../../fixtures/does-not-exist.lock'))
})
