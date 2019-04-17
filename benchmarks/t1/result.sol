{
  f:{
    file: "t1.cpp",
    heap:{
      "ex_a": "*(ex_a)",
      "ex_b": "*(ex_b)",
      "*(ex_a)": "**(ex_a)",
      "*(ex_b)": "**(ex_b)",
      "*((**(ex_a))@'p')": 42
    },
    logs:[
    ]
  },
  main:{
    file: "t1.cpp",
    heap:{
      "ex_x": 42,
      "ex_s": "*(ex_s)",
      "(*(ex_s))@'p'": "ex_x",
      "ret": 42
    },
    logs:[
    ]
  }
}
