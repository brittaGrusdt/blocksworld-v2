### Theoretic Tables

The input of the RSA-model is a **set of causal Bayes nets**, each consisting of
a probability distribution over two variables, A and C, referred to as
probability table and a causal net ('A,C independent', 'A implies C/-C' or 'C
implies A/-A') where '-' denotes 'not'. Depending on the causal net the
probability table is constructed by different conditional and marginal
probabilities. For the dependent causal nets, the respective  conditional and
marginal probabilities are sampled from a beta distribution skewed towards 1
(P(child|parent)), a beta distribution skewed towards 0 (P(child|-parent)) and
the marginal probability (P(parent) is sampled from a uniform distribution, e.g.
for 'A implies C', we sample:

  * P(C|A) ~ beta(10,1)
  * P(C|-A) ~ beta(1, 10)
  * P(A) ~ beta(1, 1)

That is, we assume the probability of the child given the parent to be rather
large, the probability of the child given the parent variable is not
present to be small and the parent variable is equally likely to have any
probability between 0 and 1.

For each of the causal nets we sample 2500 tables and for the independent causal
net 10000 to have an equal number of tables sampled "from" dependent and
independent causal nets as model input. Each of the in total 20,000 tables is
then combined with each of the five causal nets. These combinations are a priori
more or less likely (likelihood given by beta distributions), e.g. a table that
was sampled "from" the causal net 'A implies -C' will be highly unlikely under
the causal net 'A implies C' whereas it may for instance also be rather likely
under the causal net 'C implies -A'.

### Behavioral Tables

In the behavioral experiment each participant generates a probability
distribution over the two variables * A: The blue block falls * C: The green
block falls that is directly normalized in the experiment while participants
give their rating, i.e. they directly submit the normalized distribution.

For each of the 13 different stimuli we get a Dirichlet distribution that best
fits the participants' ratings for the respective stimuli. For each stimulus we
sample the same number of tables from the fitted Dirichlet distributions.

#### Generalisation (augmented) behavioral tables

Participants' slider ratings are continous values. For a better comparison, we summarize them by assuming that certain tables represent the same, more coarse-grained belief. 
Using continous values in the experiment is however useful due to the immediate normalization.

For each unique empirical table t:
  * we map each table entry (t1-t4) to the closest value divisible by 5, e.g. 87 maps to
  85, 23 to 25, 18 to 20 etc.
  * take all resulting table entry combinations and normalize them 
  * filter out those that change more than allowed, i.e. the new value must be in the range +-2 compared to the old value and make sure that the resulting set of tables contains only distinct tables
  * each table in this final set of tables is associated with the original empirical table t (empirical_id) and gets a unique table id
 * That is each empirical id may be associated with several table ids 

### Model Input

In order to generate the tables that serve as input to the model to compare the
predictions from the model with participants' ratings, we have two options:
  * we use the set of tables sampled from the fitted Dirichlet distributions 
  * we use the set of tables sampled from the theoretically assumed
  distributions for each of the causal nets.

Then we check how many of the empirical tables (including the augmented tables as described above) are included in the respective set of generated tables (In our pilot experiment roughly 60% for both).

### Model

#### Prior 

An input state consists of a causal net *cn* and a probability table *table*.

P(cn, table) = P(cn|table) * P(table)

The beta distributions used for the causal nets in the theoretical tables represent our belief how the probability tables tend to look like under the respective causal assumptions. Therefore we use these beta distributions as log likelihood functions independently of the set of tables that we use as input for the model (theoretic or tables from fitted dirichlet distributions).

The table is uniformly sampled from the set of tables.


#### Speaker

The predictions of the speaker are contingent on the *literal meaning* of a state, i.e. on how often a state is considered to be true for a given utterance.
We assume a simple literal semantics, where the utterance is true whenever its corresponding probability exceeds a given threshold theta. That is, the literal semantics does *not depend on the causal net*, only on the probability table. 
In other words, **the predictions for the speaker only depend on the probability table, not on the associated causal net**.
However, the prior probability of a table may differ largely when combined with different causal nets. 







