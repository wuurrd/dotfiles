import i3
import sys

workspaces = i3.get_workspaces()
currentnum = i3.filter(workspaces, focused=True)[0]['num']
if sys.argv[1] == 'next':
    workspace_next = i3.filter(workspaces, num=currentnum+1)
    if len(workspace_next) == 0:
        workspace_next = workspaces[0]
    else:
        workspace_next = workspace_next[0]
else:
    workspace_next = i3.filter(workspaces, num=currentnum-1)
    if len(workspace_next) == 0:
        workspace_next = workspaces[-1]
    else:
        workspace_next = workspace_next[0]
print workspace_next
ws_nodes = i3.filter(num=currentnum)[0]['nodes']
ws_nodes = ws_nodes + i3.filter(num=currentnum)[0]['floating_nodes']
curr = i3.filter(ws_nodes, focused=True)[0]
i3.move('container to workspace number %s' % workspace_next['num'])
i3.workspace('number %s' % workspace_next['num'])
# ids = [win['id'] for win in i3.filter(ws_nodes, nodes=[])]

# if sys.argv[0] == 'next':
#     next_idx = (ids.index(curr['id']) + 1) % len(ids)
# else:
#     next_idx = (ids.index(curr['id']) - 1) % len(ids)
# next_id = ids[next_idx]

# i3.focus(con_id=next_id)
