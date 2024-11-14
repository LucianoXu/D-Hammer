/* Copyright (c) 2012-2017 The ANTLR Project. All rights reserved.
 * Use of this file is governed by the BSD 3-clause license that
 * can be found in the LICENSE.txt file in the project root.
 */

//
//  main.cpp
//  antlr4-cpp-demo
//
//  Created by Mike Lischke on 13.03.16.
//

#include <iostream>
#include <map>
#include <string>

#include "ualg.hpp"


using namespace ualg;
using namespace std;

int main(int , const char **) {

  string head = "head";
  vector<const NormalTerm*> args;
  auto x = calc_hash_normal(head, {});
  printf("hash: %lu\n", x);
  return 0;
}
