
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Collections;
using System.Configuration;
using System.Web;
using System.Threading;
using System.Globalization;
using System.Text;
#endregion

public partial class view_void_apchk : System.Web.UI.Page
{
    string oldinv = "";
    string recid = "";
   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "void_ap_chklist.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            lblUser.Text = UserLogin.UserName;
            Session["Customers_Company"] = labelcompany.Text;

            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        

             


    }

   protected void FormView1_OnDataBound(object sender, EventArgs e)
   {
     
       //if (FormView1.CurrentMode == FormViewMode.insert)
       //{
       //    TextBox vendor = (TextBox)FormView1.FindControl("vendTextBox");
       //    TextBox vendorname = (TextBox)FormView1.FindControl("vendnameTextBox");
       //    TextBox checkno = (TextBox)FormView1.FindControl("chknoTextBox");
       //    TextBox date = (TextBox)FormView1.FindControl("chkdateTextBox");
       //    TextBox amt = (TextBox)FormView1.FindControl("chkamtTextBox");
       //    TextBox bank = (TextBox)FormView1.FindControl("bnkcodTextBox");
       //    TextBox bankname = (TextBox)FormView1.FindControl("bnknameTextBox");
       //    TextBox voided = (TextBox)FormView1.FindControl("voidedTextBox");
       //    TextBox reckey = (TextBox)FormView1.FindControl("vRecKeyTextBox");

       //    vendor.Focus();
       //    ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
       //   // try
       //   // {
       //        UserClass UserLogin = (UserClass)Session["User"];
       //        voucherpay con = new voucherpay();
       //        DataSet ds = new DataSet();
       //        ds = con.SelectVoidAPCheck(UserLogin.UserName, "Update", "", 0, "", "", 0, "", "", "", "", "");

       //        vendor.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
       //        vendorname.Text = Convert.ToString(ds.Tables[0].Rows[0][6]);
       //        bank.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
       //        bankname.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);
       //        checkno.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
       //        date.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);
       //        amt.Text = Convert.ToString(ds.Tables[0].Rows[0][4]);
       //        voided.Text = Convert.ToString(ds.Tables[0].Rows[0][8]);
       //        reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][10]);
               
              
       //    //}
       //   // catch { }
           
           

       //}

       if (FormView1.CurrentMode == FormViewMode.Edit)
       {
           TextBox voided = (TextBox)FormView1.FindControl("voidedTextBox");
           voided.Focus();
           
       }
       
       if (FormView1.CurrentMode == FormViewMode.ReadOnly)
       {
           
           try
           {

               Label reckey = (Label)FormView1.FindControl("vRecKeyLabel");
               Session["void_chklist_reckey_rec"] = reckey.Text.Trim();
               
           }
           catch { }
           
           
       }
   }  
   

   

   protected void UpdateButton_Click(object sender, EventArgs e)
   {
       TextBox vendor = (TextBox)FormView1.FindControl("vendTextBox");
       TextBox vendorname = (TextBox)FormView1.FindControl("vendnameTextBox");
       TextBox checkno = (TextBox)FormView1.FindControl("chknoTextBox");
       TextBox date = (TextBox)FormView1.FindControl("chkdateTextBox");
       TextBox amt = (TextBox)FormView1.FindControl("chkamtTextBox");
       TextBox bank = (TextBox)FormView1.FindControl("bnkcodTextBox");
       TextBox bankname = (TextBox)FormView1.FindControl("bnknameTextBox");
       TextBox voided = (TextBox)FormView1.FindControl("voidedTextBox");
       TextBox reckey = (TextBox)FormView1.FindControl("vRecKeyTextBox");

       if (checkno.Text == "")
           checkno.Text = "0";
       if (amt.Text == "")
           amt.Text = "0";

       UserClass UserLogin = (UserClass)Session["User"];
       ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
       ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
       ObjectDataSource1.SelectParameters["prmvend"].DefaultValue = vendor.Text.Trim();
       ObjectDataSource1.SelectParameters["prmchkno"].DefaultValue = checkno.Text.Trim();
       ObjectDataSource1.SelectParameters["prmbnkcod"].DefaultValue = bank.Text.Trim();
       ObjectDataSource1.SelectParameters["prmchkdate"].DefaultValue = date.Text.Trim();
       ObjectDataSource1.SelectParameters["prmchkamt"].DefaultValue = amt.Text.Trim();
       ObjectDataSource1.SelectParameters["prmvendname"].DefaultValue = vendorname.Text.Trim();
       ObjectDataSource1.SelectParameters["prmbnkname"].DefaultValue = bankname.Text.Trim();
       ObjectDataSource1.SelectParameters["prmvoided"].DefaultValue = voided.Text.Trim();
       ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = reckey.Text.Trim();
       

       FormView1.ChangeMode(FormViewMode.ReadOnly);
       

   }

   

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }


    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_void_apchk.aspx");
    }
    protected void lnk_listinvoice(object sender, EventArgs e)
    {
        Response.Redirect("void_ap_chklist.aspx");
    }

    


    protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }
    

    

    

    

    
    
    

}
