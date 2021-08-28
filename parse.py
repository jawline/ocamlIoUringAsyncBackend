import re
from datetime import datetime, timedelta

mbs_list = []
scores_thread = []
scores_uring = []

with open('./results') as f:
    d = "Creating "
    data = f.read().split(d)
    for segment in data:
        segment = d + segment
        #print("MBS :", segment)
        cpt = re.search(r'Creating ([0-9]+)', segment)
        if cpt != None:
            mbs = cpt.group(1)
            matches = re.findall(r'Elapsed \(wall clock\) time \(h:mm:ss or m:ss\): .+', segment)
            if len(matches) == 2:
                print(mbs)
                print(matches[0].split(' ')[-1])
                mbs_list.append(float(mbs) / 1000 )
                scores_thread.append(datetime.strptime(matches[0].split(' ')[-1],"%M:%S.%f"))
                scores_uring.append(datetime.strptime(matches[1].split(' ')[-1],"%M:%S.%f"))

def ts(t):
    return timedelta(hours=t.hour, minutes=t.minute, seconds=t.second, microseconds=t.microsecond).total_seconds()

for i in range(0, len(scores_thread)):
    print(mbs_list[i], ": ", ts(scores_thread[i]), ts(scores_uring[i]))

import matplotlib.pyplot as plt
import numpy as np

# Data for plotting
fig, ax = plt.subplots()

legend = ax.legend()
ax.plot(np.array(mbs_list), np.array(scores_thread), label = "Async_unix")
ax.plot(np.array(mbs_list), np.array(scores_uring), label = "io_uring")
ax.set(xlabel='Filesize (GB)', ylabel='')
ax.grid()

fig.set_size_inches(18.5, 6.5)
fig.savefig('result.png', dpi=300)
