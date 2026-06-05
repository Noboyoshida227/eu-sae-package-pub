EU SAE PACKAGE — GUIDANCE NOTE AND SUPPORTING MATERIALS
=======================================================

This folder collects the guidance note for producing poverty maps with the
EU SAE Dashboard, together with the technical notes and key papers it relies on.

CONTENTS
--------

Guidance Note on Producing Poverty Maps.docx
   The main operational guidance note for area-level small area estimation of
   poverty using the univariate (UFH) and multivariate (MFH) Fay–Herriot models
   and the EU SAE Dashboard. Annex G summarizes the methods; the full technical
   notes are in the "Technical notes" folder below.

Technical notes\
   Note on Benchmarking - UFH and MFH.docx
      Region-level ratio benchmarking, the bootstrap MSE of benchmarked
      estimates, and the cross-time mean cross-product error (MCPE).
      Corresponds to Annex G.6–G.7 of the guidance note.

   Note on eblupMFH2_robust.R.docx
      The constrained-REML safeguard for boundary (zero variance-component)
      MFH2 fits. Corresponds to Annex G.9 of the guidance note.

   Note on Estimating the Sampling-Error Covariance Matrix.docx
      The cross-time sampling-error covariance: why it arises (the persistent
      cluster effect), why panel-only estimation understates it, and the
      Dashboard's covariance options. Corresponds to Annex G.8 of the
      guidance note.

Key papers\
   Harmening et al (2023) - Area-Level SAE in R (emdi).pdf
      Harmening, S., Kreutzmann, A.-K., Schmidt, S., Salvati, N., and Schmid, T.
      (2023). A Framework for Producing Small Area Estimates Based on Area-Level
      Models in R. The R Journal, 15(1), 316–341.

   Molina & Romero (2025) - Comparable Small Area Estimates over Time.pdf
      Molina, I. and Romero, E. Comparable Small Area Estimates over Time.
      Working document.

Compiled June 2026.
