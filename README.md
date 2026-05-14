# Texas-Volleyball

This repository highlights a selection of volleyball analytics visualizations developed to support coaching and scouting decisions. Each plot is accompanied by a brief writeup covering what the analysis shows and how to interpret the specific example displayed. The work spans serve strategy, passing efficiency, and setter decision-making, with the goal of translating data-driven insights into language and visuals that are accessible to coaches at any level of analytics experience.

<p align="center">
  <kbd>
  <img width="783" height="447" alt="image" src="https://github.com/user-attachments/assets/67f44aa2-3800-47e7-881f-587c18f7a1a8" />
  </kbd>
</p>

**Potential applications:** This visualization maps the court and color-codes each location by how likely a team is to win the point when the pass lands there. Green indicates high value, red indicates low value, and the contour lines group areas of similar value — think of it like a topographic map of offensive efficiency off the first ball. Coaches can use this to set data-informed passing targets, identify which zones are costing their team points, or evaluate individual passers by tracking where their passes typically land.

**This example's takeaway:** No surprises here. The highest-value zone sits a few feet off the net, slightly right of center, which is the traditional passing target for good reason. Value drops off gradually but consistently as passes drift toward the net, push wide, or fall deep into the court. Based on nearly 100,000 observations, even a modest drift out of the green zone carries a measurable cost to offensive efficiency.

<p align="center">
  <kbd>
  <img width="700" height="542" alt="image" src="https://github.com/user-attachments/assets/a0f8997a-c83b-4d0e-9591-1d10961c4a84" />
  </kbd>
</p>

**Potential applications:** Similar to the passing map, this visualization applies the same expected point value framework but shifts the focus to where attacks are launched from on the court. Coaches can filter by team, individual setter, and specific play type to see which attack locations produce the best results, helping inform how setters distribute the ball and where hitters should be positioned.

**This example's takeaway:** This view is filtered to the V6 (Red) — an in-system set to the opposite on the right side. The red X marks the conventional in-system target (3 feet in, 3 feet off the net), but the green zone tells a slightly different story: the optimal attack location sits a touch deeper and further off the net than that traditional benchmark. Delivering the ball right on top of the net at the antenna creates more risk than reward. A set that gives the hitter a bit more room to work with tends to produce better outcomes across the nearly 8,000 observations captured here.

<p align="center">
  <kbd>
  <img width="699" height="488" alt="image" src="https://github.com/user-attachments/assets/0d343e75-6548-4cb8-83d4-9b7bb6092254" />
  </kbd>
</p>

**Potential applications:** This tool combines serve location, landing zone, and expected sideout rate (xSO) to help identify where a server should target against a specific opponent in a specific rotation. The colored arrows represent serve clusters, with red indicating the opponent is more likely to sideout and green indicating more favorable outcomes for the serving team. The gray arrows show the full distribution of all serves for context. Filtering by server, opponent, and setter rotation makes this a practical scouting tool for serve strategy preparation.

**This example's takeaway:** This view shows where Emma Halter tends to serve and how those serves perform against Kentucky when their setter is in rotation 1. A few things stand out. Serves originating from the right side of the court tend to produce better outcomes for Halter. Meanwhile, serves to the left side of the receiving court are both rare and costly when attempted — which makes sense given that All-American Molly Tuozzo occupies that passing zone in this rotation. The data suggests the most favorable approach may be serving down the line from the right side, attacking the seam away from Tuozzo while still pressuring Kentucky's system.

<p align="center">
  <kbd>
  <img width="698" height="368" alt="image" src="https://github.com/user-attachments/assets/233e4144-78e8-4c7b-aab4-3752f1e9514e" />
  </kbd>
</p>

**Potential applications:** This chart establishes a blueprint for effective serving by first isolating above-average servers in the dataset, then grouping them by aggression level based on their combined ace and error rate. Each horizontal bar represents the average pass outcome distribution for that group, creating a reference profile for what successful serving looks like at each aggression level. A individual server can then be plotted against these benchmarks to understand how their serving profile compares and where their value comes from.

**This example's takeaway:** Averi Carlson's ace and error rates place her in the low-aggression tier, and her profile largely mirrors what successful low-aggression servers look like. What sets her apart is her out-of-system rate. She is forcing poor passes at a rate that rivals higher-aggression servers without taking on the added risk that comes with that style. Her xSO sitting below the 0.588 baseline confirms she is impacting rallies from the service line. The combination of low error risk and consistent pressure on the passer is what makes her serving profile difficult to replicate.
