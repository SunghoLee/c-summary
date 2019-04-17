{
  getCharFromString:{
    file: "b9.cpp",
    heap:{
      "ex_string": "*(ex_string)"
    },
    logs:[
      {
        ln: "25:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_string)",
          "im_arg2": 0
        }
      }
    ]
  },
  native_send:{
    file: "b9.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_data": "*(ex_data)"
    },
    logs:[
      {
        ln: "25:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_data)",
          "im_arg2": 0
        }
      }
    ]
  },
  registerNativeMethods:{
    file: "b9.cpp",
    heap:{
      "ex_clazz": "im_46:13:ret",
      "ex_className": "*(ex_className)"
    },
    logs:[
      {
        ln: "46:13",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "*(ex_className)"
        }
      },
      {
        ln: "50:9",
        jfun: "_JNIEnv_RegisterNatives",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_46:13:ret",
          "im_arg2": "*(ex_gMethods)",
          "im_arg3": "*(ex_numMethods)"
        }
      }
    ]
  },
  registerNatives:{
    file: "b9.cpp",
    heap:{
    },
    logs:[
      {
        ln: "46:13",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'org/arguslab/native_leak_dynamic_register/MainActivity'"
        }
      },
      {
        ln: "50:9",
        jfun: "_JNIEnv_RegisterNatives",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_46:13:ret",
          "im_arg2": "gb_gMethods",
          "(*((gb_gMethods)@0))@'name'": "'send'",
          "(*((gb_gMethods)@0))@'signature'": "'(Ljava/lang/String;)V'",
          "(*((gb_gMethods)@0))@'fnPtr'": "fn_native_send",
          "im_arg3": "im_62:10"
        }
      }
    ]
  },
  JNI_OnLoad:{
    file: "b9.cpp",
    heap:{
    },
    logs:[
      {
        ln: "46:13",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'org/arguslab/native_leak_dynamic_register/MainActivity'"
        }
      },
      {
        ln: "50:9",
        jfun: "_JNIEnv_RegisterNatives",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_46:13:ret",
          "im_arg2": "gb_gMethods",
          "(*((gb_gMethods)@0))@'name'": "'send'",
          "(*((gb_gMethods)@0))@'signature'": "'(Ljava/lang/String;)V'",
          "(*((gb_gMethods)@0))@'fnPtr'": "fn_native_send",
          "im_arg3": "im_62:10"
        }
      }
    ]
  }
}
