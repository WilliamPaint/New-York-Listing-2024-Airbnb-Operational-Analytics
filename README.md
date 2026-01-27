<h1>Airbnb Market Analysis: Pricing, Prediction &amp; Optimization (NYC, 2024)</h1>
<img width="651" height="473" alt="image" src="https://github.com/user-attachments/assets/0a86da3b-832f-4f7a-9397-01976f899e99" />

<div style="margin:12px 0;">
  <span style="background:#0f172a;color:#ffffff;padding:6px 10px;border-radius:6px;font-size:13px;font-weight:600;">
    Operational Analytics Project :
  </span>
  <span style="background:#1f2937;color:#ffffff;padding:6px 10px;border-radius:6px;font-size:13px;font-weight:600;margin-left:6px;">
    Descriptive • Predictive • Prescriptive
  </span>

</div>

<hr>

<!-- ================= OVERVIEW ================= -->
<div style="background:#f3f4f6;padding:8px 12px;border-left:5px solid #6366f1;margin:18px 0 10px 0;">
  <strong>Overview</strong>
</div>

<p>
  This project develops an end-to-end operational analytics framework to support
  data-driven Airbnb pricing decisions in New York City. Using the 2024 NYC Airbnb
  dataset, the analysis integrates descriptive, predictive, and prescriptive analytics
  to understand market structure, forecast nightly prices, and identify optimal
  geographic pricing strategies.
</p>

<p>
  The project moves beyond intuition-based pricing by combining machine learning
  and spatial optimization to produce actionable recommendations for hosts and
  platform decision-makers.
</p>

<!-- ================= BUSINESS PROBLEM ================= -->
<div style="background:#f3f4f6;padding:8px 12px;border-left:5px solid #ef4444;margin:18px 0 10px 0;">
  <strong>Business Problem</strong>
</div>

<p>
  Airbnb prices in NYC vary widely due to neighborhood effects, property attributes,
  host behavior, and regulatory constraints. Many hosts rely on intuition or simple
  comparisons, leading to underpricing or lost occupancy. This project addresses:
</p>

<ul>
  <li>What does the NYC Airbnb pricing landscape look like?</li>
  <li>Which listing attributes best predict nightly price?</li>
  <li>How can predictive insights be translated into an optimal pricing strategy?</li>
</ul>

<!-- ================= DATA ================= -->
<div style="background:#f3f4f6;padding:8px 12px;border-left:5px solid #eab308;margin:18px 0 10px 0;">
  <strong>Data</strong>
</div>

<ul>
  <li>
    <strong>Source:</strong>
    NYC Airbnb Open Data (2024) — Kaggle  
    <br>
    <span style="font-size:13px;color:#6b7280;">
      https://www.kaggle.com/datasets/vrindakallu/new-york-dataset
    </span>
  </li>
  <li><strong>Scope:</strong> Thousands of active listings across all five boroughs</li>
  <li><strong>Key variables:</strong>
    <ul>
      <li>Pricing: nightly price, minimum nights, availability</li>
      <li>Property: room type, bedrooms, property type</li>
      <li>Host: review counts, response behavior, host listings</li>
      <li>Location: latitude, longitude, neighborhood group</li>
    </ul>
  </li>
</ul>

<!-- ================= DESCRIPTIVE ================= -->
<div style="background:#f3f4f6;padding:8px 12px;border-left:5px solid #9ca3af;margin:18px 0 10px 0;">
  <strong>Descriptive Analytics</strong>
</div>

<ul>
  <li>Summary statistics, histograms, boxplots, and cumulative distributions</li>
  <li>Outlier detection using z-scores and 1.5×IQR</li>
  <li>Correlation analysis and missing data diagnostics</li>
  <li><strong>Key finding:</strong> ~90% of listings priced below $300</li>
</ul>

<!-- ================= PREDICTIVE ================= -->
<div style="background:#f3f4f6;padding:8px 12px;border-left:5px solid #2563eb;margin:18px 0 10px 0;">
  <strong>Predictive Analytics</strong>
</div>

<ul>
  <li>Linear Regression (VIF-pruned)</li>
  <li>Decision Tree (1-SE pruning)</li>
  <li>Random Forest (600 trees, OOB tuning)</li>
  <li><strong>Best model:</strong> Random Forest (RMSE ≈ 113, R² ≈ 0.31)</li>
</ul>

<!-- ================= PRESCRIPTIVE ================= -->
<div style="background:#f3f4f6;padding:8px 12px;border-left:5px solid #16a34a;margin:18px 0 10px 0;">
  <strong>Prescriptive Analytics</strong>
</div>

<ul>
  <li>Spatial optimization using Random Forest predictions</li>
  <li>Objective: maximize distance-weighted predicted price</li>
  <li>Optimal anchor: Midtown Manhattan (~1.75-mile radius)</li>
</ul>

<!-- ================= TOOLS ================= -->
<div style="background:#f3f4f6;padding:8px 12px;border-left:5px solid #06b6d4;margin:18px 0 10px 0;">
  <strong>Tools &amp; Environment</strong>
</div>

<ul>
  <li>R, RStudio</li>
  <li>tidyverse, rpart, randomForest</li>
  <li>Visualization &amp; spatial analysis libraries</li>
</ul>

<!-- =================MY CONTRIBUTIONS ================= -->
<div style="background:#f3f4f6;padding:8px 12px;border-left:5px solid #db2777;margin:18px 0 10px 0;">
  <strong>Contributions</strong>
</div>

<p>
  This project was completed as a <strong>group assignment</strong> for an Operational Analytics course.
  My individual contributions included:
</p>

<ul>
  <li>Data cleaning and exploratory analysis in R</li>
  <li>Predictive modeling and evaluation</li>
  <li>Prescriptive optimization</li>
  <li>Analytical writing and reporting</li>
</ul>

<!-- ================= RECOMMENDATIONS ================= -->
<div style="background:#f3f4f6;padding:8px 12px;border-left:5px solid #22c55e;margin:18px 0 10px 0;">
  <strong>Practical Recommendations</strong>
</div>

<ul>
  <li>Emphasize value-segment pricing (&lt; $300/night)</li>
  <li>Treat luxury listings as a separate pricing niche</li>
  <li>Integrate ML-driven pricing guidance</li>
  <li>Use geographic pricing anchors</li>
</ul>
