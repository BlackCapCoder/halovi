import sys, json;
from neovim import attach

nvim = attach('child', argv=["/bin/env", "nvim", "--embed", "-u", "NONE"])
inp  = json.load(sys.stdin)

for reg in inp["registers"]:
    nvim.command('let @' + reg['name'] + ' = "' + reg['value'].replace('"', '\\"') + '"')

nvim.current.buffer[:] = inp['buffer'].splitlines()
nvim.input(inp['input'])

sys.stdout.write("\n".join(nvim.current.buffer))
