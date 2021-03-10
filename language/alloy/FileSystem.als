
// A file system object in the file system
sig FSObject { parent: lone Dir }

// A directory in the file system
sig Dir extends FSObject { contents: set FSObject }

// A file in the file system
sig File extends FSObject { }

// A directory is the parent of its contents
fact { all d: Dir, o: d.contents | o.parent = d }

// All file system objects are either files or directories
fact { File + Dir = FSObject }

// There exists a root
one sig Root extends Dir { } { no parent }

// in: "subset of"
//  * : 反射推移閉包(reflexive transitive closure)
// File system is connected
fact { FSObject in Root.*contents }

// ^ : 推移閉包、自分自身を含まない
// asyclic : 非循環
// assert
// The contents path is acyclic
assert acyclic { no d: Dir | d in d.^contents }

// Now check it for a scope of 5
// 反例を探す
// check: 反例を探す
// check <> for <number>
check acyclic for 5

// File system has one root
assert oneRoot { one d: Dir | no d.parent }

// Now check it for a scope of 5
check oneRoot for 5

// Every fs object is in at most one directory
assert oneLocation { all o: FSObject | lone d: Dir | o in d.contents }

// Now check it for a scope of 5
check oneLocation for 5

// an assertion with a counterexample
 assert Wrong {
   all obj, p: (FSObject - Root) | (obj.parent = p.parent)
 }

 check Wrong for 3
