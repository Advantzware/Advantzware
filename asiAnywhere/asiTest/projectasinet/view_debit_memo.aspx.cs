
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

public partial class view_debit_memo : System.Web.UI.Page
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
            string vPage = "debit_memo.aspx";
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

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {

                if (Convert.ToString(Session["debit_memo_add_button"]) == "add")
                {
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["debit_memo_add_button"] = null;
                }
                if (Session["view_debit_memo_reckey_index"] != null)
                {
                    try
                    {
                        GridView1.SelectedIndex = Convert.ToInt32(Session["view_debit_memo_reckey_index"]);
                        Session["view_debit_memo_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }
                else
                {
                    try
                    {
                        GridView1.SelectedIndex = 0;
                        Session["view_debit_memo_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }

            }
            

        } //  ! Page.IsPostBack

             


    }

   protected void FormView1_OnDataBound(object sender, EventArgs e)
   {
     
       if (FormView1.CurrentMode == FormViewMode.Insert)
       {
           TextBox vendor = (TextBox)FormView1.FindControl("vendnoTextBox");
           Label vendorname = (Label)FormView1.FindControl("vendnameLabel");
           TextBox memono = (TextBox)FormView1.FindControl("memonumTextBox");
           TextBox date = (TextBox)FormView1.FindControl("memodateTextBox");
           TextBox amt = (TextBox)FormView1.FindControl("amtTextBox");
           TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");

           vendor.Focus();
           ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
           try
           {
               UserClass UserLogin = (UserClass)Session["User"];
               voucherpay con = new voucherpay();
               DataSet ds = new DataSet();
               ds = con.SelectDbCrMemo("AddNewRecord", "", UserLogin.UserName, "", "", 0, "", 0, 0, "");

               vendor.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
               vendorname.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
               memono.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
               date.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);
               amt.Text = Convert.ToString(ds.Tables[0].Rows[0][4]);
               reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][6]);
               
              
           }
           catch { }
           
           FormView2.Visible = false;
           GridView1.Visible = false;
           AddNewFormView2Button.Visible = false;

       }
       if (FormView1.CurrentMode == FormViewMode.Edit)
       {
           TextBox vendor = (TextBox)FormView1.FindControl("vendnoTextBox");           
           vendor.Focus();
           FormView2.Visible = false;
           GridView1.Visible = false;
       }
       if (FormView1.CurrentMode == FormViewMode.ReadOnly)
       {
           
           try
           { 
               
               Label reckey = (Label)FormView1.FindControl("reckeyLabel");
               Session["debit_memo_reckey_rec"] = reckey.Text.Trim();
               
           }
           catch { }
           FormView2.Visible = true;
           GridView1.Visible = true;
           
       }
   }
   
   protected void delete_Button_Click(object sender, EventArgs e)
   {

       Label reckey = (Label)FormView1.FindControl("reckeyLabel");
       UserClass UserLogin = (UserClass)Session["User"];

       try
       {
           ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
           ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "DataDelete";
           ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

           FormView1.ChangeMode(FormViewMode.ReadOnly);

           Response.Write("<script>window.location.href='view_debit_memo.aspx'</script>");
       }
       catch { }

   }

   protected void Formview1_InsertCancelButtonClick(object sender, EventArgs e)
   {
       try
       {
           TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");
           UserClass UserLogin = (UserClass)Session["User"];
           voucherpay con = new voucherpay();
           DataSet ds = new DataSet();
           ds = con.SelectDbCrMemo("DataDelete", "", UserLogin.UserName, "", "", 0, "", 0, 0, reckey.Text.Trim());

       }
       catch { }

   }

   protected void UpdateButton_Click(object sender, EventArgs e)
   {
       TextBox vendor = (TextBox)FormView1.FindControl("vendnoTextBox");
       Label vendorname = (Label)FormView1.FindControl("vendnameLabel");
       TextBox memono = (TextBox)FormView1.FindControl("memonumTextBox");
       TextBox date = (TextBox)FormView1.FindControl("memodateTextBox");
       TextBox amt = (TextBox)FormView1.FindControl("amtTextBox");
       if (memono.Text == "")
           memono.Text = "0";
       if (amt.Text == "")
           amt.Text = "0";

       UserClass UserLogin = (UserClass)Session["User"];
       ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
       ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
       ObjectDataSource1.SelectParameters["prmvend"].DefaultValue = vendor.Text.Trim();
       ObjectDataSource1.SelectParameters["prmvname"].DefaultValue = vendorname.Text.Trim();
       ObjectDataSource1.SelectParameters["prmcheckno"].DefaultValue = memono.Text.Trim();
       ObjectDataSource1.SelectParameters["prmcheckdate"].DefaultValue = date.Text.Trim();
       ObjectDataSource1.SelectParameters["prmamt"].DefaultValue = amt.Text.Trim();
       

       FormView1.ChangeMode(FormViewMode.ReadOnly);
       

   }

   protected void InsertButton_Click(object sender, EventArgs e)
   {
       TextBox vendor = (TextBox)FormView1.FindControl("vendnoTextBox");
       Label vendorname = (Label)FormView1.FindControl("vendnameLabel");
       TextBox memono = (TextBox)FormView1.FindControl("memonumTextBox");
       TextBox date = (TextBox)FormView1.FindControl("memodateTextBox");
       TextBox amt = (TextBox)FormView1.FindControl("amtTextBox");
       TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");
       if (memono.Text == "")
           memono.Text = "0";
       if (amt.Text == "")
           amt.Text = "0";

       UserClass UserLogin = (UserClass)Session["User"];
       
       voucherpay val = new voucherpay();

       bool check = val.ValidateDbCrMemo("ValidateAdd", "", UserLogin.UserName, vendor.Text.Trim(), vendorname.Text.Trim(), Convert.ToInt32(memono.Text.Trim()), "", Convert.ToDecimal(amt.Text.Trim()), 0, "");

            string value = Convert.ToString(check);
            if (value == "True")
            {
                Session["debit_memo_reckey_rec"] = reckey.Text.Trim();
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
                ObjectDataSource1.SelectParameters["prmvend"].DefaultValue = vendor.Text.Trim();
                ObjectDataSource1.SelectParameters["prmvname"].DefaultValue = vendorname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcheckno"].DefaultValue = memono.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcheckdate"].DefaultValue = date.Text.Trim();
                ObjectDataSource1.SelectParameters["prmamt"].DefaultValue = amt.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);
                FormView2.ChangeMode(FormViewMode.Insert);

            }
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
        Response.Redirect("view_debit_memo.aspx");
    }
    protected void lnk_listinvoice(object sender, EventArgs e)
    {
        Response.Redirect("debit_memo.aspx");
    }

    

    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["view_debit_memo_reckey_index"] = GridView1.SelectedIndex;
        Session["view_debit_memo_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
    }

    protected void UpdateButton_Formview2_Click(object sender, EventArgs e)
    {
        TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
        TextBox duedate = (TextBox)FormView2.FindControl("duedateLabel");
        TextBox baldue = (TextBox)FormView2.FindControl("baldueLabel");
        TextBox cramt = (TextBox)FormView2.FindControl("cramtTextBox");
        TextBox dramt = (TextBox)FormView2.FindControl("dbamtTextBox");
        TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");        

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        if (baldue.Text == "")
            baldue.Text = "0";
        if (cramt.Text == "")
            cramt.Text = "0";
        if (dramt.Text == "")
            dramt.Text = "0";
        UserClass UserLogin = (UserClass)Session["User"];
        
        voucherpay val = new voucherpay();

        bool check = val.ValidateViewDbCrMemo("UpdateValidate", "", UserLogin.UserName, invno.Text.Trim(), duedate.Text.Trim(), Convert.ToDecimal(baldue.Text.Trim()), Convert.ToDecimal(cramt.Text.Trim()), Convert.ToDecimal(dramt.Text.Trim()), act.Text.Trim(), "", Convert.ToString(Session["view_debit_memo_reckey"]), "", Convert.ToString(Session["debit_memo_reckey_rec"]));

            string value = Convert.ToString(check);
            if (value == "True")
            {
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Update";
                //ObjectDataSource3.SelectParameters["prmComp"].DefaultValue =  ; 
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prminvno"].DefaultValue = invno.Text.Trim(); ;
                ObjectDataSource3.SelectParameters["prmduedate"].DefaultValue = duedate.Text.Trim();
                ObjectDataSource3.SelectParameters["prmamtdue"].DefaultValue = baldue.Text.Trim();
                ObjectDataSource3.SelectParameters["prmamtpaid"].DefaultValue = cramt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmamtdisc"].DefaultValue = dramt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactnum"].DefaultValue = act.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactdscr"].DefaultValue = actdscr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_debit_memo.aspx'</script>");
            }
    }

    protected void AddButton_Formview2_Click(object sender, EventArgs e)
    {

        TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
        TextBox duedate = (TextBox)FormView2.FindControl("duedateLabel");
        TextBox baldue = (TextBox)FormView2.FindControl("baldueLabel");
        TextBox cramt = (TextBox)FormView2.FindControl("cramtTextBox");
        TextBox dramt = (TextBox)FormView2.FindControl("dbamtTextBox");
        TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel"); 
        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        if (baldue.Text == "")
            baldue.Text = "0";
        if (cramt.Text == "")
            cramt.Text = "0";
        if (dramt.Text == "")
            dramt.Text = "0";
        UserClass UserLogin = (UserClass)Session["User"];

        voucherpay val = new voucherpay();

        bool check = val.ValidateViewDbCrMemo("UpdateValidate", "", UserLogin.UserName, invno.Text.Trim(), duedate.Text.Trim(), Convert.ToDecimal(baldue.Text.Trim()), Convert.ToDecimal(cramt.Text.Trim()), Convert.ToDecimal(dramt.Text.Trim()), act.Text.Trim(), "", Convert.ToString(Session["view_debit_memo_reckey"]), "", Convert.ToString(Session["debit_memo_reckey_rec"]));

            string value = Convert.ToString(check);
            if (value == "True")
            {

                Session["view_debit_memo_reckey"] = reckey.Text.Trim();
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Update";
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prminvno"].DefaultValue = invno.Text.Trim(); ;
                ObjectDataSource3.SelectParameters["prmduedate"].DefaultValue = duedate.Text.Trim();
                ObjectDataSource3.SelectParameters["prmamtdue"].DefaultValue = baldue.Text.Trim();
                ObjectDataSource3.SelectParameters["prmamtpaid"].DefaultValue = cramt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmamtdisc"].DefaultValue = dramt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactnum"].DefaultValue = act.Text.Trim();
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_debit_memo.aspx'</script>");

            }
        

    }

    protected void FormView2_OnDataBound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
            TextBox duedate = (TextBox)FormView2.FindControl("duedateLabel");
            TextBox baldue = (TextBox)FormView2.FindControl("baldueLabel");
            TextBox cramt = (TextBox)FormView2.FindControl("cramtTextBox");
            TextBox dramt = (TextBox)FormView2.FindControl("dbamtTextBox");
            TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
            TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");


            TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
            invno.Focus();

            

            UserClass UserLogin = (UserClass)Session["User"];
            try
            {
                voucherpay val = new voucherpay();

                bool check = val.ValidateViewDbCrMemo("AddnewRecValidate", "", UserLogin.UserName, "", "", 0, 0, 0, "", "", "", "", Convert.ToString(Session["debit_memo_reckey_rec"]));

                string value = Convert.ToString(check);
                if (value == "True")
                {
                    voucherpay con = new voucherpay();
                    DataSet ds = new DataSet();
                    ds = con.SelectViewDbCrMemo("Add", "", UserLogin.UserName, "", "", 0, 0, 0, "", "", "", "", Convert.ToString(Session["debit_memo_reckey_rec"]));

                    invno.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
                    duedate.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
                    baldue.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
                    cramt.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);

                    dramt.Text = Convert.ToString(ds.Tables[0].Rows[0][4]);
                    act.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);
                    actdscr.Text = Convert.ToString(ds.Tables[0].Rows[0][6]);

                    reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);
                    GridView1.Visible = false;
                }


                else
                {
                    FormView2.ChangeMode(FormViewMode.ReadOnly);
                }
            }
                catch { }

        }
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
            invno.Focus();
            GridView1.Visible = false;
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
                Session["view_debit_memo_reckey"] = reckey.Text.Trim();

            }
            catch { }
            GridView1.Visible = true;
            
            try
            {
                if (FormView2.DataItemCount.ToString() == "0")
                    AddNewFormView2Button.Visible = true;
                else
                    AddNewFormView2Button.Visible = false;
            }
            catch { }
        }
    }
    protected void CancelButton_FormView2_Delete(object sender, EventArgs e)
    {
        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            voucherpay con = new voucherpay();
            DataSet ds = new DataSet();
            ds = con.SelectViewDbCrMemo("CancelDelete", "001", UserLogin.UserName, "", "", 0, 0, 0, "", "", reckey.Text.Trim(), "", "");
        }
        catch { }
        FormView1.Visible = true;
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='view_debit_memo.aspx'</script>");
    }
    protected void deleteButton_FormView2_Click(object sender, EventArgs e)
    {
       
        UserClass UserLogin = (UserClass)Session["User"];
        Label reckey = (Label)FormView2.FindControl("ReckeyLabel");

        
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Delete";
                ObjectDataSource3.SelectParameters["prmapmain"].DefaultValue = Convert.ToString(Session["debit_memo_reckey_rec"]);
                Response.Write("<script>window.location.href='view_debit_memo.aspx'</script>");
            

    }

    protected void AddNewFormView2Button_Click(object sender, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Insert);
        AddNewFormView2Button.Visible = false;
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

    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);

    }

    protected void Invoice_OnTextChange(object sender, EventArgs e)
    {

        TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
        TextBox duedate = (TextBox)FormView2.FindControl("duedateLabel");
        TextBox baldue = (TextBox)FormView2.FindControl("baldueLabel");
        TextBox cramt = (TextBox)FormView2.FindControl("cramtTextBox");
        TextBox dramt = (TextBox)FormView2.FindControl("dbamtTextBox");
        TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");
        Label vendor = (Label)FormView1.FindControl("vendnoLabel");
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            voucherpay con = new voucherpay();
            DataSet ds = new DataSet();
            ds = con.SelectInvoiceInfolook("Search", UserLogin.UserName, "Invoice","EQUAL",invno.Text.Trim(), vendor.Text.Trim());


            //sqft.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
            duedate.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
            baldue.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);
            cramt.Text = Convert.ToString(ds.Tables[0].Rows[0][6]);
            dramt.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);
            act.Text = Convert.ToString(ds.Tables[0].Rows[0][8]);
            actdscr.Text = Convert.ToString(ds.Tables[0].Rows[0][9]);
            cramt.Focus();
        }
        catch { }

    }

    

    

    
    
    

}
