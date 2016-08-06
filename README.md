Illustration of the delta-method
================================

I keep forgetting how to implement the delta-method in R. So, here is an example using the deltamethod function from the [msm package](https://cran.r-project.org/web/packages/msm/index.html). The example deals with an occupancy model: I assume that initial occupancy was estimated along with its standard error (SE), and that one would like to obtain the SE of subsequent occupancy probabilities. Further examples can be obtained by typing in example(deltamethod) in R.

For a nice introduction to the delta-method, check [that](http://www.phidot.org/software/mark/docs/book/pdf/app_2.pdf) out.
