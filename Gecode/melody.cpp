#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/int/arithmetic.hh>
#include <gecode/set.hh>
#include <vector>

using namespace Gecode;

namespace {
  int majorNatural[7] = {2, 2, 1, 2, 2, 2, 1};
  int minorNatural[7] = {2, 1, 2, 2, 2, 1, 2};

  int progression[4] = {1, 5, 6, 4};
};

class Melody : public Script {
private:

  const int bars = 4;
  const int quantification = 4*48;

  SetVarArray push;
  SetVarArray pull;
  SetVarArray playing;

public:

  Melody(const SizeOptions& opt) :
    Script(opt),
    push(*this,bars*quantification+1,IntSet::empty,IntSet(0,127),0,127),
    pull(*this,bars*quantification+1,IntSet::empty,IntSet(0,127),0,127),
    playing(*this,bars*quantification+1,IntSet::empty,IntSet(0,127),0,127){

      // Array of 127 sets containing for each note the time where it is pushed (or pulled)
      SetVarArray pushMap(*this, 127, IntSet::empty,IntSet(0,bars*quantification+1),0,bars*quantification+1);
      SetVarArray pullMap(*this, 127, IntSet::empty,IntSet(0,bars*quantification+1),0,bars*quantification+1);
      channel(*this, push, pushMap);
      channel(*this, pull, pullMap);
      for (int i = 0; i < 127; i++){
        rel(*this, cardinality(pushMap[i]) == cardinality(pullMap[i]));
      }

      int scaleSize = sizeof(majorNatural)/sizeof(int);
      int progressionSize = sizeof(progression)/sizeof(int);


      rel(*this, pull[0] == IntSet::empty);
      rel(*this, push[bars*quantification] == IntSet::empty);
      rel(*this, playing[bars*quantification] == IntSet::empty);
      rel(*this, playing[0] == push[0]);

      for(int i = 1; i <= bars*quantification; i++){
        // Notes that are playing
        rel(*this, playing[i] == ((playing[i-1] - pull[i]) | push[i]));
        // Cannot pull a note not playing
        rel(*this, pull[i] <= playing[i-1]);
        // Cannot push a note still playing
        rel(*this, push[i] || (playing[i-1] - pull[i]));
      }

      cardinality(*this, playing, 0, 5);
      cardinality(*this, pull, 0, 10);
      cardinality(*this, push, 0, 10);


      // Following a scale

      std::vector<int> v;
      for (int octave = 0; octave < 11; octave++){
        int offset = 0;
        for (int i = 0; i<scaleSize; i++){
          v.push_back(octave*12+offset);
          offset += majorNatural[i];
        }
      }

      IntArgs a(v);
      IntSet scaleSet(a);

      for(int i = 0; i <= bars*quantification; i++){
        rel(*this, push[i] <= scaleSet);
      }

      // Following a chord progression

      for (int i = 0; i < progressionSize; i++){
        std::vector<int> v;
        for (int octave = 0; octave < 11; octave++){
          int offset = 0;
          for (int j = 0; j<scaleSize; j++){
            if (j == progression[i]-1){
              v.push_back(octave*12+offset);
            } else if (j == (progression[i]+1)%scaleSize){
              v.push_back(octave*12+offset);
            } else if (j == (progression[i]+3)%scaleSize){
              v.push_back(octave*12+offset);
            }
            offset += majorNatural[j];
          }
        }

        IntArgs a(v);
        IntSet chordSet(a);
        for(int j = 0; j < bars*quantification/progressionSize; j++){
          rel(*this, push[j + i*bars*quantification/progressionSize] <= chordSet);
        }
      }

      // Limiting the range of notes
      dom(*this, push, SRT_SUB, 0, 12);

      // Constraining the min length of the notes
      int minlength = 24;
      for (int i = 0; i <= bars*quantification; i++){
        for (int j = 1; j < minlength && i+j <= bars*quantification ; j++){
          rel(*this, pull[i+j] || push[i]);
        }
      }

      // Constraining the max length of the notes
      /* int maxlength = 24;
      for (int i = 0; i < bars*quantification; i++){
        for (int j = 1; j < maxlength && i+j < bars*quantification ; j++){

        }
      }  */

      // Constraining chord rhythm
      for (int i = 0; i < bars*quantification; i++){
        if (i % 24 == 0){
          cardinality(*this, push[i], 3, 3);
        } else {
          cardinality(*this, push[i], 0, 1);
        }
      }

      Rnd r1(opt.seed());
      r1.time();
      Rnd r2(opt.seed());
      r2.time();

      for (int i = 0; i <= bars*quantification; i++){
        branch(*this, push[i], SET_VAL_RND_INC(r2));
        branch(*this, pull[i], SET_VAL_RND_INC(r2));
      }



  }

  /// Constructor for cloning \a s
  Melody(Melody& melody) :
    Script(melody){

      push.update(*this, melody.push);
      pull.update(*this, melody.pull);
      playing.update(*this, melody.playing);

  }

  /// Copy during cloning
  virtual Space*
  copy(void) {
    return new Melody(*this);
  }
  /// Print solution
  virtual void
  print(std::ostream& os) const {

    os << "\t";
    for (int i = 0; i<=bars*quantification; i++) {
      os << "Beat " << i << "    ";
      for (SetVarGlbValues d(push[i]);d();++d) {
        os << d.val() << " ";
      };
      os << std::endl << "\t";
    }
    os << std::endl << "\t";
    for (int i = 0; i<=bars*quantification; i++) {
      os << "Beat " << i << "    ";
      for (SetVarGlbValues d(pull[i]);d();++d) {
        os << d.val() << " ";
      };
      os << std::endl << "\t";
    }
    os << std::endl << "\t";
    for (int i = 0; i<=bars*quantification; i++) {
      os << "Beat " << i << "    ";
      for (SetVarGlbValues d(playing[i]);d();++d) {
        os << d.val() << " ";
      };
      os << std::endl << "\t";
    }
    os << std::endl;
  }
};

/** \brief Main-function
 *  \relates melody
 */
int main(int argc, char* argv[]) {
  SizeOptions opt("Melody");
  opt.solutions(1);
  opt.parse(argc,argv);
  Script::run<Melody,DFS,SizeOptions>(opt);
  return 0;
}
