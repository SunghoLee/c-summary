{
  Java_org_arguslab_native_1source_1clean_MainActivity_sourceClean:{
    file: "b23.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_data": "*(ex_data)"
    },
    logs:[
      {
        ln: "20:31",
        jfun: "_JNIEnv_GetObjectClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "*(ex_data)"
        }
      },
      {
        ln: "21:28",
        jfun: "_JNIEnv_GetFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_20:31:ret",
          "im_arg2": "'data'",
          "im_arg3": "'Ljava/lang/String;'"
        }
      },
      {
        ln: "22:25",
        jfun: "_JNIEnv_NewStringUTF",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'Clean the data in the native'"
        }
      },
      {
        ln: "23:5",
        jfun: "_JNIEnv_SetObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(ex_data)",
          "im_arg2": "im_21:28:ret",
          "im_arg3": "im_22:25:ret"
        }
      }
    ]
  }
}
