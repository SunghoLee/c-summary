{
  getCharFromString:{
    file: "b16.cpp",
    heap:{
      "ex_string": "*(ex_string)"
    },
    logs:[
      {
        ln: "143:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "*(ex_string)",
          "im_arg2": 0
        }
      }
    ]
  },
  getImei:{
    file: "b16.cpp",
    heap:{
      "ex_context": "*(ex_context)"
    },
    logs:[
      {
        ln: "147:18",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/content/Context'"
        }
      },
      {
        ln: "148:21",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_147:18:ret",
          "im_arg2": "'getSystemService'",
          "im_arg3": "'(Ljava/lang/String;)Ljava/lang/Object;'"
        }
      },
      {
        ln: "150:20",
        jfun: "_JNIEnv_GetStaticFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_147:18:ret",
          "im_arg2": "'TELEPHONY_SERVICE'",
          "im_arg3": "'Ljava/lang/String;'"
        }
      },
      {
        ln: "152:29",
        jfun: "_JNIEnv_GetStaticObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_147:18:ret",
          "im_arg2": "im_150:20:ret"
        }
      },
      {
        ln: "153:25",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(ex_context)",
          "im_arg2": "im_148:21:ret",
          "im_arg3": "im_152:29:ret"
        }
      },
      {
        ln: "154:11",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/telephony/TelephonyManager'"
        }
      },
      {
        ln: "155:11",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_154:11:ret",
          "im_arg2": "'getDeviceId'",
          "im_arg3": "'()Ljava/lang/String;'"
        }
      },
      {
        ln: "156:30",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_153:25:ret",
          "im_arg2": "im_155:11:ret"
        }
      }
    ]
  },
  handle_input:{
    file: "b16.cpp",
    heap:{
      "ex_event": "*(ex_event)",
      "ex_context": "*(*(*(*(*(ex_app)))@'activity'))@'clazz'"
    },
    logs:[
      {
        ln: "147:18",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/content/Context'"
        }
      },
      {
        ln: "148:21",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_147:18:ret",
          "im_arg2": "'getSystemService'",
          "im_arg3": "'(Ljava/lang/String;)Ljava/lang/Object;'"
        }
      },
      {
        ln: "150:20",
        jfun: "_JNIEnv_GetStaticFieldID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_147:18:ret",
          "im_arg2": "'TELEPHONY_SERVICE'",
          "im_arg3": "'Ljava/lang/String;'"
        }
      },
      {
        ln: "152:29",
        jfun: "_JNIEnv_GetStaticObjectField",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_147:18:ret",
          "im_arg2": "im_150:20:ret"
        }
      },
      {
        ln: "153:25",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(*(*(*(*(ex_app)))@'activity'))@'clazz'",
          "im_arg2": "im_148:21:ret",
          "im_arg3": "im_152:29:ret"
        }
      },
      {
        ln: "154:11",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/telephony/TelephonyManager'"
        }
      },
      {
        ln: "155:11",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_154:11:ret",
          "im_arg2": "'getDeviceId'",
          "im_arg3": "'()Ljava/lang/String;'"
        }
      },
      {
        ln: "156:30",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_153:25:ret",
          "im_arg2": "im_155:11:ret"
        }
      },
      {
        ln: "143:12",
        jfun: "_JNIEnv_GetStringUTFChars",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_156:30:ret",
          "im_arg2": 0
        }
      }
    ]
  }
}
