# test check_inputs

test_data <- data.frame(y = c(1,1,2,2),
                        t = c(1,2,3,2),
                        id = c(1,1,1,2),
                        g = c(0,0,0,2),
                        u = c(1,1,1,1))

check_inputs(test_data, yname="y", tname="t", idname="id", gname = "g", unitname= "un")
