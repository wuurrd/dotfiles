import sys

import i3

outputs = i3.get_outputs()
outputs = i3.filter(outputs, active=True)
workspaces = i3.get_workspaces()
currentfocus = i3.filter(workspaces, focused=True)[0]['output']

current = 0
for i, workspace in enumerate(outputs):
    if workspace['name'] == currentfocus:
        current = i
next_workspace = current + 1
if outputs[next_workspace % len(outputs)]['name'] == 'xroot-0':
    next_workspace += 1

i3.move('workspace to output %s' % outputs[next_workspace % len(outputs)]['name'])
