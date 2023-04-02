local dap = require('dap')

dap.adapters.go = {
  type = 'executable';
  command = 'node';
  args = {os.getenv('HOME') .. '/projects/vscode-go/dist/debugAdapter.js'};
}
dap.configurations.go = {
  {
    type = 'go',
    name = 'Debug',
    request = 'launch',
    showLog = true,
    program = "${file}",
    cwd = "${workspaceFolder}",
    args = {"./tmp-trails/ec2-pretty.json"},
    dlvToolPath = vim.fn.exepath('/Users/alice.rum/go/bin/dlv')  -- Adjust to where delve is installed
  },
}

