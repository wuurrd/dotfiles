import i3
import sys

num = i3.filter(i3.get_workspaces(), focused=True)[0]["num"]
ws_nodes = i3.filter(num=num)[0]["nodes"]
ws_nodes = ws_nodes + i3.filter(num=num)[0]["floating_nodes"]
curr = i3.filter(ws_nodes, focused=True)[0]

ids = [win["id"] for win in i3.filter(ws_nodes, nodes=[])]

if sys.argv[1] == "next":
    next_idx = (ids.index(curr["id"]) + 1) % len(ids)
else:
    next_idx = (ids.index(curr["id"]) - 1) % len(ids)
next_id = ids[next_idx]

i3.focus(con_id=next_id)
