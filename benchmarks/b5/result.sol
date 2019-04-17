{
  getCharFromString:{
    file: "b5.cpp",
    heap:{
      "ex_string": "*(ex_string)"
    },
    logs:[
      {
        ln: "27:12",
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
    file: "b5.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_data": "*(ex_data)"
    },
    logs:[
      {
        ln: "27:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_data)",
          "im_arg2": 0
        }
      }
    ]
  },
  native_sendFoo:{
    file: "b5.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_index": "*(ex_index)",
      "ex_data": "*(ex_data)"
    },
    logs:[
      {
        ln: "27:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_data)",
          "im_arg2": 0
        }
      }
    ]
  },
  native_sendBar:{
    file: "b5.cpp",
    heap:{
      "ex_thisObj": "*(ex_thisObj)",
      "ex_dw": "*(ex_dw)",
      "ex_data": "*(ex_data)"
    },
    logs:[
      {
        ln: "27:12",
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
    file: "b5.cpp",
    heap:{},
    logs:[
      {
        ln: "60:13",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "*(ex_className)"
        }
      },
      {
        ln: "64:9",
        jfun: "_JNIEnv_RegisterNatives",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_60:13:ret",
          "im_arg2": "*(ex_gMethods)",
          "im_arg3": "*(ex_numMethods)"
        }
      }
    ]
  },
  regiterNatives:{
    file: "b5.cpp",
    heap:{},
    logs:[
      {
        ln: "60:13",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'org/arguslab/native_dynamic_register_multiple/MainActivity'"
        }
      },
      {
        ln: "64:9",
        jfun: "_JNIEnv_RegisterNatives",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_60:13:ret",
          "im_arg2": "gb_gMethods",
          "(*(gb_gMethods@0))@'name'": "'send'",
          "(*(gb_gMethods@0))@'signature'": "'(Ljava/lang/String;)V'",
          "(*(gb_gMethods@0))@'fnPtr'": "fn_native_send",
          "(*(gb_gMethods@1))@'name'": "'sendFoo'",
          "(*(gb_gMethods@1))@'signature'": "'(ILjava/lang/String;)V'",
          "(*(gb_gMethods@1))@'fnPtr'": "fn_native_sendFoo",
          "(*(gb_gMethods@2))@'name'": "'sendBar'",
          "(*(gb_gMethods@2))@'signature'": "'(DLjava/lang/String;)V'",
          "(*(gb_gMethods@2))@'fnPtr'": "fn_native_sendBar",
          "im_arg3": "im_76:10"
        }
      }
    ]
  },
  JNI_OnLoad:{
    file: "b5.cpp",
    heap:{},
    logs:[
      {
        ln: "60:13",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'org/arguslab/native_dynamic_register_multiple/MainActivity'"
        }
      },
      {
        ln: "64:9",
        jfun: "_JNIEnv_RegisterNatives",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_60:13:ret",
          "im_arg2": "gb_gMethods",
          "(*(gb_gMethods@0))@'name'": "'send'",
          "(*(gb_gMethods@0))@'signature'": "'(Ljava/lang/String;)V'",
          "(*(gb_gMethods@0))@'fnPtr'": "fn_native_send",
          "(*(gb_gMethods@1))@'name'": "'sendFoo'",
          "(*(gb_gMethods@1))@'signature'": "'(ILjava/lang/String;)V'",
          "(*(gb_gMethods@1))@'fnPtr'": "fn_native_sendFoo",
          "(*(gb_gMethods@2))@'name'": "'sendBar'",
          "(*(gb_gMethods@2))@'signature'": "'(DLjava/lang/String;)V'",
          "(*(gb_gMethods@2))@'fnPtr'": "fn_native_sendBar",
          "im_arg3": "im_76:10"
        }
      }
    ]
  }
}
