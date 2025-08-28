=== Manual Replication (classical approach)

→ *Technical frictions*

- *Setting up an identical software environment*: replicability relies on the ability to reproduce the same execution context (OS, interpreter, libraries). However, some modules I encountered were obsolete or undocumented. The exact reconstruction then becomes a complex task, conditioned on access to specific or sometimes non-public versions.
- *Moving or incomplete datasets*: micro-modifications made throughout the experiment (e.g., correction in Excel, deleted columns) without clear history, causing unexplained divergences.
- *Sensitivity to implicit statistical details*: high number of unmentioned degrees of freedom (normalization methods, outlier handling, choice of optimizer in GLMM), shifting small “hidden” choices into large result discrepancies.

→ *Human frictions*

- *High cognitive load*: understanding another researcher’s reasoning through text is often more complex than expected: renamed variables, implicit assumptions, linguistic shortcuts or digressions make decoding long and difficult.
- *Reconstruction time*: manual rewriting follows a sequential logic: read → understand → code → test. This pipeline is time-consuming but is still the traditional method many people continue to use, limiting the number of iterations achievable and thus the probability of reproducing the “correct” workflow by chance. With experience, this chance turns into intuition, allowing an individual to become highly efficient.
- *Interpretive bias*: personal interpretation of unclear areas influences my reconstruction choices when multiple interpretations are possible. This introduces a personal bias in a process meant to eliminate such bias. This problem accumulates across works, since each paper is written by a different person with their own writing style, meaning that an interpretation may lead to the right conclusions for one paper but completely opposite conclusions for another.

→ *Organizational frictions*

- *Incomplete or missing documentation*: scripts without README, cryptic variable names, or missing version indications. I should add that some team code is stored in Excel cells where line breaks are replaced by “/”, and the general experiment workflow has not been documented—critical details to know, for example, whether our dataset fits the code (which was not always the case at the beginning of the work).
- *Loss of scientific history*: impossible to trace *which version of which script* generated *which result*, further hindering replicability attempts. Generally, one only has access to part of the latest version of the team’s code, yet the reported results are not always linked to that version, leading to misleading conclusions.



=== Replication assisted by LLM

→ *Technical frictions*

- *Hallucinations and approximate completions*: the LLM can generate “plausible” steps not present in the paper due to lack of information. However, this can be useful if used well. By generating several “plausible” codes, one can evaluate each and compare results with the group to determine the most similar code.
- *Ambiguities*: when the source text is unclear, due to the author’s difficulty in expressing certain ideas in language, the LLM makes implicit assumptions, compromising fidelity. However, the same evaluation technique as above can be used to mitigate this lack of information.
- *Partial non-determinism*: two identical calls to the model may produce different codes, making replication difficult. This could be solved by controlling hyperparameters like temperature. But then we lose the possibility of using the greedy approach, requiring a strategy to adjust hyperparameters as needed. For now, since hyperparameters cannot be controlled in LLM assistance, this remains a friction point.

→ *Human frictions*

- *Over-confidence*: after repeatedly using LLMs and seeing their efficiency compared to the manual stage, I started to blindly trust them, which is harmful since the LLM is supposed to assist, not replace, the human.
- *Prompt engineering time cost*: compensating for the LLM’s lack of context requires long, structured, iterative prompts, which paradoxically can be longer and more costly than coding directly. Since LLMs have their own habits, often not aligned with ours, we may produce code we understand less than if we had written it ourselves—making situations where the LLM fails to find a solution terribly blocking.

→ *Organizational frictions*

- *Confidentiality of data sent to the LLM*: institutional issues arise when sending sensitive data to external APIs. It is worth remembering that these LLMs do not belong to us, and sending sensitive data is a bad practice in this field. One must check clauses before using them, or consider running a local LLM, which brings its own issues.
- *Weak traceability of interactions*: without logging, discussions are not archived, undermining scientific auditability. Good methodology is needed to avoid chaotic use of LLMs as in daily life.



=== Replication via LLM + API pipeline

→ *Technical frictions*

- *Integration complexity*: chaining OCR → parsing → prompting → execution → scoring forms a fragile pipeline where each link can fail. Results will not always be conclusive, so human assistance is still needed to check for errors.
- *API usage limits (cost, latency, quota)*: multiple calls while searching for good parameters quickly drive up costs. Free APIs could be used, but they degrade code quality.
- *Environment issues*: generated code may require different versions or may simply not run on the current machine. This complicates evaluating all generated code without restrictions. One can request generation within a given environment, but then reproducing results elsewhere becomes impossible, undermining true replicability.

→ *Human frictions*

- *Very diverse technological skills required*: since the pipeline is supposed to generate all the code, humans must analyze problems when they arise, requiring broad technical knowledge as errors can occur in many areas.
- *Complex debugging*: an error in a microservice or malformed JSON requires tracing back through the entire chain, and once the problem is found, it is often hard to fix. The current structure is rigid, making debugging difficult. That said, my pipeline is still basic compared to what could be developed in the future.

→ *Organizational frictions*

- *Cost and maintenance of dedicated infrastructure*: this requires choices between using external LLM APIs (budget for requests) or running locally (necessary hardware). If it becomes too costly, the project loses its value.
- *Risk of rapid obsolescence*: API versions change and components become obsolete, rendering the pipeline unusable in a few months if not maintained. The problem is that we don’t control every part of the pipeline, which is problematic and requires solutions.
