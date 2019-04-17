{
  Java_org_arguslab_native_1set_1field_1from_1arg_MainActivity_setField:{
    file: "b19.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_complexData": "*(ex_complexData)",
      "ex_foo": "*(ex_foo)"
    },
    logs:[
      {
        ln: "28:31",
        jfun: "_JNIEnv_GetObjectClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "*(ex_complexData)"
        }
      },
      {
        ln: "29:27",
        jfun: "_JNIEnv_GetFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_28:31:ret",
          "im_arg2": "'foo'",
          "im_arg3": "'Lorg/arguslab/native_set_field_from_arg/Foo;'"
        }
      },
      {
        ln: "31:5",
        jfun: "_JNIEnv_SetObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(ex_complexData)",
          "im_arg2": "im_29:27:ret",
          "im_arg3": "*(ex_foo)"
        }
      },
      {
        ln: "32:22",
        jfun: "_JNIEnv_GetObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_complexData)",
          "im_arg2": "im_29:27:ret"
        }
      }
    ]
  }
}
