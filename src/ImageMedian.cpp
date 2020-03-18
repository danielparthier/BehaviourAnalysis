//Functions to extract median matrix from image stack
#include <RcppArmadillo.h>
#include <omp.h>
#define ARMA_USE_LAPACK
#define ARMA_NO_DEBUG

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat ImageMedian(arma::cube& x, int& cores) {
  omp_set_num_threads(cores);
  //arma::cube tmp_mat = arma::cube(x.memptr(), dimX, dimY, dimZ, false, false);
  int dimX = x.n_rows;
  int dimY = x.n_cols;
  arma::mat OutputImage = arma::mat(dimX, dimY, arma::fill::zeros);
#pragma omp parallel for shared(OutputImage) schedule(static) 
  for(int i = 0; i < dimX; ++i) {
    for(int j = 0; j < dimY; ++j) {
      arma::vec tmp_vec = x.tube(i, j);
      OutputImage(i, j) = arma::median(tmp_vec);
    }  
  }
  return OutputImage;
}