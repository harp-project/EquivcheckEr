# Wrangler Refactoring Equivalence Checker (PoC)

- Uses property based testing to check the equivalence of code refactored by Wrangler
- Compares commits of the same codebase with the original and refactored code (git)

# Todo

- [ ] Find/create libgit bindigs (currently an OS process is spawned with the CLI tool)
- [ ] Load modules into separate nodes to avoid reloading while testing
