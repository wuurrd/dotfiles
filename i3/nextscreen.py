import i3
import sys

workspaces = i3.get_workspaces()
visible_workspaces = i3.filter(workspaces, visible=True)
current = 0
for i, w in enumerate(visible_workspaces):
    if w['focused']:
        current = i
workspace_next = visible_workspaces[(current+1) % len(visible_workspaces)]
i3.workspace('number %s' % workspace_next['num'])
# ids = [win['id'] for win in i3.filter(ws_nodes, nodes=[])]

# if sys.argv[0] == 'next':
#     next_idx = (ids.index(curr['id']) + 1) % len(ids)
# else:
#     next_idx = (ids.index(curr['id']) - 1) % len(ids)
# next_id = ids[next_idx]

# i3.focus(con_id=next_id)
