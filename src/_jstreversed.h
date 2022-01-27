#ifndef __JSTREVERSED_H
#define __JSTREVERSED_H

#include <RcppArmadillo.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "polya_fit_simple.h"
#include <math.h>
#include <vector>
#include <algorithm>
#include <utility>
#include <numeric>


class modeljstrev {

public:
  modeljstrev(void) {};
  ~modeljstrev(void) {
    if (dfm) delete dfm;
  };

  int numiters;
  int numTopics;
  int numSentiLabs;
  int numDocs;
  int vocabSize;
  int updateParaStep;

  double aveDocSize;
  double alpha_;
  double beta_;
  double gamma_;

  arma::sp_mat * dfm;
  std::vector<int> docSizes;
  std::map<int,int> sentiLex;

  std::vector<std::vector<int> > topic_dw;
  std::vector<std::vector<int> > sent_dw;

  // model counts
  std::vector<int> nd;
  std::vector<std::vector<int> > ndz;
  std::vector<std::vector<std::vector<int> > > ndlz;
  std::vector<std::vector<std::vector<int> > > nlzw;
  std::vector<std::vector<int> > nlz;

  //Posterior values
  std::vector<std::vector<double> > p_lz;

  // hyperParameters
  std::vector<std::vector<double> > alpha_dz;
  std::vector<double> alphaSum_d;
  std::vector<std::vector<std::vector<double> > > beta_lzw;
  std::vector<std::vector<double> > betaSum_lz;
  std::vector<std::vector<double> > gamma_zl;
  std::vector<double> gammaSum_z;

  //Result vectors
  std::vector<std::vector<std::vector<double> > > pi_zdl; // size: (numDocs x L)
	std::vector<std::vector<double> > theta_zd; // size: (numDocs x L x T)
	std::vector<std::vector<std::vector<double> > > phi_zlw; // size: (L x T x V)

  void init(Rcpp::IntegerVector& sentiWords, Rcpp::IntegerVector& sentiCategory);
  void init_estimate();
  int estimate();

  std::vector<std::vector<std::vector<double> > >  returnPi() { return pi_zdl;};
  std::vector<std::vector<double> > returnTheta() { return theta_zd;};
  std::vector<std::vector<std::vector<double> > > returnPhi() { return phi_zlw;};
  std::vector<std::vector<std::vector<double> > > termScores();

private:
  void init_parameters();
  void drawsample(int document, int wordToken, int&topic, int&sentilab);
  void set_alpha();
  void set_beta();
  void set_gamma();
  void compute_pi_zdl();
  void compute_theta_zd();
  void compute_phi_zlw();
  void update_Parameters();

};

#endif
