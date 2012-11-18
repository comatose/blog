#include <vector>
#include <iostream>
#include <utility>
#include <algorithm>
#include <cstdint>
#include <bitset>
#include <string>
#include <functional>

using namespace std;

typedef struct {
  int _destiny;
  vector<pair<int, int>> _clearings;
} Problem;

istream& operator>>(istream& in, Problem& p){
  in >> p._destiny;
  p._clearings.resize(p._destiny + 1);
  for(int i = 1; i < p._destiny; ++i){
    in >> p._clearings[i].first;
    in >> p._clearings[i].second;
  }
  p._clearings[p._destiny] = {p._destiny, p._destiny};
}

class Scene{
public:
  uint64_t _passed;
  int _pos;
  bitset<48> _state;

  void step1(const Problem& p){
    _state.flip(_pos);
    _pos = (_state[_pos])?p._clearings[_pos].first:p._clearings[_pos].second;
    _passed++;
  }
};

string solve(const Problem& p){
  Scene alice{0, 1}, bob{0, 1};
  bob.step1(p);
  while(alice._pos != bob._pos || alice._state != bob._state){
    if(alice._pos == p._destiny)
      return to_string(alice._passed);
    alice.step1(p); bob.step1(p); bob.step1(p);
  }
  return "Infinity";
}

int main(){
  int iter;
  cin >> iter;
  for(int i = 1; i <= iter; ++i){
    Problem p;
    cin >> p;
    cout << "Case #" << i << ": " << solve(p) << endl;
  }
  return EXIT_SUCCESS;
}
