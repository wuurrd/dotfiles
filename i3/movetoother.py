import i3
import sys

outputs = i3.get_outputs()
outputs = i3.filter(outputs, active=True)
outputs = [o["name"] for o in outputs]
if len(outputs) == 1:
    sys.exit(0)
workspaces = i3.get_workspaces()

candidates = [1, 2, 3]

other_workspace = [o for o in outputs if o not in ["eDP1", "xroot-0"]][0]

for w in workspaces:
    if w["num"] not in candidates:
        continue
    if w["output"] == "eDP1":
        i3.move("workspace to output %s" % (other_workspace), workspace=w["name"])

# for workspace in workspaces:
# 	if workspace['output']
