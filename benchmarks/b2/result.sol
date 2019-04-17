{
  Java_org_arguslab_icc_1nativetojava_MainActivity_sendIntent:{
    heap:{
      "ex_intent": "im_17:21:ret",
      "ex_constructor": "im_18:29:ret",
      "ex_i": "im_26:9:ret",
      "ex_scnid": "im_20:23:ret",
      "ex_pseid": "im_24:23:ret",
      "ex_context": "im_27:22:ret",
      "ex_said": "im_28:22:ret"
    },
    logs:[
      {
        ln: "17:21",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/content/Intent'"
        }
      },
      {
        ln: "18:29",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_17:21:ret",
          "im_arg2": "'<init>'",
          "im_arg3": "'()V'"
        }
      },
      {
        ln: "19:17",
        jfun: "_JNIEnv_NewObject",
        args: ["im_arg0", "im_arg1", "im_arg2"],
        heap: {
          "im_arg1": "im_17:21:ret",
          "im_arg2": "im_18:29:ret"
        }
      },
      {
        ln: "20:23",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_17:21:ret",
          "im_arg2": "'setClassName'",
          "im_arg3": "'(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;'"
        }
      },
      {
        ln: "22:41",
        jfun: "_JNIEnv_NewStringUTF",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'org.arguslab.icc_nativetojava'"
        }
      },
      {
        ln: "23:31",
        jfun: "_JNIEnv_NewStringUTF",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'org.arguslab.icc_nativetojava.FooActivity'"
        }
      },
      {
        ln: "22:9",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3", "im_arg4"],
        heap: {
          "im_arg1": "im_19:17:ret",
          "im_arg2": "im_20:23:ret",
          "im_arg3": "im_22:41:ret",
          "im_arg4": "im_23:31:ret"
        }
      },
      {
        ln: "24:23",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"], 
        heap: {
          "im_arg1": "im_17:21:ret",
          "im_arg2": "'putExtra'",
          "im_arg3": "'(Ljava/lang/String;Ljava/lang/String;)Landroid/content/Intent;'"
        }
      },
      {
        ln: "26:41",
        jfun: "_JNIEnv_NewStringUTF",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'data'"
        }
      },
      {
        ln: "26:9",
        jfun: "_JNIEnv_CallObjectMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3", "im_arg4"], 
        heap: {
          "im_arg1": "im_22:9:ret",
          "im_arg2": "im_24:23:ret",
          "im_arg3": "im_26:41:ret",
          "im_arg4": "*(ex_data)"
        }
      },
      {
        ln: "27:22",
        jfun: "_JNIEnv_FindClass",
        args: ["im_arg0", "im_arg1"],
        heap: {
          "im_arg1": "'android/content/Context'"
        }
      },
      {
        ln: "28:22",
        rloc: "im_ret",
        jfun: "_JNIEnv_GetMethodID",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "im_27:22:ret",
          "im_arg2": "'startActivity'",
          "im_arg3": "'(Landroid/content/Intent;)V'"
        }
      },
      {
        ln: "29:5",
        jfun: "_JNIEnv_CallVoidMethod",
        args: ["im_arg0", "im_arg1", "im_arg2", "im_arg3"],
        heap: {
          "im_arg1": "*(ex_thisObj)",
          "im_arg2": "im_28:22:ret",
          "im_arg3": "im_26:9:ret"
        }
      }
    ]
  }
}
