## Function file for enzKinet2 package

#' Make Curve
#'
#' @author Haig Bishop
#' 28/06/2024
#'
#' Generates a "curve" for an enzyme kinetics model. The "curve" is just a dense
#' dataframe for the model.
#' @param model The chosen model ("MM", "MMSI", etc.)
#' @param params The parameters for the model (Including Km, Vmax, Ksi, etc.)
#' @param x.min Defines the range of x values to cover.
#' @param x.max Defines the range of x values to cover.
#' @param z.values Defines the z values to cover (NULL if not used)
#' @param n_samples The resolution of the curve (128 is almost always enough)
#' @param space The distribution of the space to generate data
#' @return curve.df
#' 
#' @export

make_curve <- function(model, params, x.min, x.max, z.values = NULL, n_samples = 250, space = "linear") {
    
    # Make use of simulate_data to generate data with 0 noise
    curve.df <- simulate_data(model, params, x.min, x.max, z.values = z.values, 
                              noise_level = 0, n_samples = n_samples, space = space)
    
    return(curve.df)
}
