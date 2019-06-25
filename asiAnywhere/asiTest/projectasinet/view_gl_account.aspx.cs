
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class view_gl_account : System.Web.UI.Page
{


    protected void Page_Load(object sender, System.EventArgs e)
    {

        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_gl_account.aspx";
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

            //RadioButtonList1.SelectedIndex = 0;

            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;
                Session["customer_user_id"] = UserLogin.UserName;

                if (Convert.ToString(Session["gl_account_list_add_button"]) == "add")
                {
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["gl_account_list_add_button"] = null;
                }


            }

            //if (cyr1TextBox.text == "")
            //{
            //    cyr1TextBox.text = "0.00";
            //}
            
        }
        
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label reckey = (Label)FormView1.FindControl("RecKeyLabel");
                Session["gl_account_list_reckey"] = reckey.Text.Trim();
            }
            catch { }
            Label chklabel = (Label)FormView1.FindControl("checkLabel");
            Label acctype = (Label)FormView1.FindControl("actypeLabel");
            RadioButtonList rd1 = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
            CheckBox chk = (CheckBox)FormView1.FindControl("CheckBox2");
            if (chklabel.Text == "yes")
                chk.Checked = true;
            if (acctype.Text == "A" || acctype.Text == "a")
                rd1.SelectedIndex = 0;
            if (acctype.Text == "C" || acctype.Text == "c")
                rd1.SelectedIndex = 1;
            if (acctype.Text == "E" || acctype.Text == "e")
                rd1.SelectedIndex = 2;
            if (acctype.Text == "L" || acctype.Text == "l")
                rd1.SelectedIndex = 3;
            if (acctype.Text == "R" || acctype.Text == "r")
                rd1.SelectedIndex = 4;
            if (acctype.Text == "T" || acctype.Text == "t")
                rd1.SelectedIndex = 5;
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            Label chklabel = (Label)FormView1.FindControl("checkLabel");
            CheckBox chk = (CheckBox)FormView1.FindControl("CheckBox2");
            if (chklabel.Text == "yes")
                chk.Checked = true;
        }

    }
    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
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


    

    protected void addButton_Click(object sender, EventArgs e)
    {

        TextBox curopn = (TextBox)FormView1.FindControl("cry_opnTextBox");
        TextBox lstopn = (TextBox)FormView1.FindControl("lyr_opnTextBox");
        TextBox accnum = (TextBox)FormView1.FindControl("actTextBox");
        TextBox desc = (TextBox)FormView1.FindControl("actdscrTextBox");

        Label cyr1 = (Label)FormView1.FindControl("cyr1TextBox");
        Label cyr2 = (Label)FormView1.FindControl("cyr2TextBox");
        Label cyr3 = (Label)FormView1.FindControl("cyr3TextBox");
        Label cyr4 = (Label)FormView1.FindControl("cyr4TextBox");
        Label cyr5 = (Label)FormView1.FindControl("cyr5TextBox");
        Label cyr6 = (Label)FormView1.FindControl("cyr6TextBox");
        Label cyr7 = (Label)FormView1.FindControl("cyr7TextBox");
        Label cyr8 = (Label)FormView1.FindControl("cyr8TextBox");
        Label cyr9 = (Label)FormView1.FindControl("cyr9TextBox");
        Label cyr10 = (Label)FormView1.FindControl("cyr10TextBox");
        Label cyr11 = (Label)FormView1.FindControl("cyr11TextBox");
        Label cyr12 = (Label)FormView1.FindControl("cyr12TextBox");
        Label cyr13 = (Label)FormView1.FindControl("cyr13TextBox");

        Label lyr1 = (Label)FormView1.FindControl("lyr1TextBox");
        Label lyr2 = (Label)FormView1.FindControl("lyr2TextBox");
        Label lyr3 = (Label)FormView1.FindControl("lyr3TextBox");
        Label lyr4 = (Label)FormView1.FindControl("lyr4TextBox");
        Label lyr5 = (Label)FormView1.FindControl("lyr5TextBox");
        Label lyr6 = (Label)FormView1.FindControl("lyr6TextBox");
        Label lyr7 = (Label)FormView1.FindControl("lyr7TextBox");
        Label lyr8 = (Label)FormView1.FindControl("lyr8TextBox");
        Label lyr9 = (Label)FormView1.FindControl("lyr9TextBox");
        Label lyr10 = (Label)FormView1.FindControl("lyr10TextBox");
        Label lyr11 = (Label)FormView1.FindControl("lyr11TextBox");
        Label lyr12 = (Label)FormView1.FindControl("lyr12TextBox");
        Label lyr13 = (Label)FormView1.FindControl("lyr13TextBox");

        TextBox bud1 = (TextBox)FormView1.FindControl("bud1TextBox");
        TextBox bud2 = (TextBox)FormView1.FindControl("bud2TextBox");
        TextBox bud3 = (TextBox)FormView1.FindControl("bud3TextBox");
        TextBox bud4 = (TextBox)FormView1.FindControl("bud4TextBox");
        TextBox bud5 = (TextBox)FormView1.FindControl("bud5TextBox");
        TextBox bud6 = (TextBox)FormView1.FindControl("bud6TextBox");
        TextBox bud7 = (TextBox)FormView1.FindControl("bud7TextBox");
        TextBox bud8 = (TextBox)FormView1.FindControl("bud8TextBox");
        TextBox bud9 = (TextBox)FormView1.FindControl("bud9TextBox");
        TextBox bud10 = (TextBox)FormView1.FindControl("bud10TextBox");
        TextBox bud11 = (TextBox)FormView1.FindControl("bud11TextBox");
        TextBox bud12 = (TextBox)FormView1.FindControl("bud12TextBox");
        TextBox bud13 = (TextBox)FormView1.FindControl("bud13TextBox");

        TextBox ly_bud1 = (TextBox)FormView1.FindControl("ly_bud1TextBox");
        TextBox ly_bud2 = (TextBox)FormView1.FindControl("ly_bud2TextBox");
        TextBox ly_bud3 = (TextBox)FormView1.FindControl("ly_bud3TextBox");
        TextBox ly_bud4 = (TextBox)FormView1.FindControl("ly_bud4TextBox");
        TextBox ly_bud5 = (TextBox)FormView1.FindControl("ly_bud5TextBox");
        TextBox ly_bud6 = (TextBox)FormView1.FindControl("ly_bud6TextBox");
        TextBox ly_bud7 = (TextBox)FormView1.FindControl("ly_bud7TextBox");
        TextBox ly_bud8 = (TextBox)FormView1.FindControl("ly_bud8TextBox");
        TextBox ly_bud9 = (TextBox)FormView1.FindControl("ly_bud9TextBox");
        TextBox ly_bud10 = (TextBox)FormView1.FindControl("ly_bud10TextBox");
        TextBox ly_bud11 = (TextBox)FormView1.FindControl("ly_bud11TextBox");
        TextBox ly_bud12 = (TextBox)FormView1.FindControl("ly_bud12TextBox");
        TextBox ly_bud13 = (TextBox)FormView1.FindControl("ly_bud13TextBox");

        RadioButtonList type = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
        CheckBox trm = (CheckBox)FormView1.FindControl("CheckBox2");

        UserClass UserLogin = (UserClass)Session["User"];

        //contact_list c1 = new contact_list();
        //bool check = c1.validatecustomer1(Convert.ToString(Session["User"]), "Validate", state.Text, zip.Text, city.Text, type.Text, desctype.Text, sman.Text, descsman.Text, terms.Text, descterms.Text, currcode.Text, loc.Text, descloc.Text, carrier.Text, descarrier.Text, delzone.Text, deszone.Text, terr.Text, desterr.Text, taxgr.Text);

        //string chec = Convert.ToString(check);

        //if (chec == "True")
        //{

        //    try
        //    {
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";                
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmact"].DefaultValue = accnum.Text.Trim();
                ObjectDataSource1.SelectParameters["prmactdscr"].DefaultValue = desc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmactype"].DefaultValue = type.SelectedValue;
                ObjectDataSource1.SelectParameters["prmcry_opn"].DefaultValue = curopn.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr_opn"].DefaultValue = lstopn.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr1"].DefaultValue = cyr1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr2"].DefaultValue = cyr2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr3"].DefaultValue = cyr3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr4"].DefaultValue = cyr4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr5"].DefaultValue = cyr5.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr6"].DefaultValue = cyr6.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr7"].DefaultValue = cyr7.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr8"].DefaultValue = cyr8.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr9"].DefaultValue = cyr9.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr10"].DefaultValue = cyr10.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr11"].DefaultValue = cyr11.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr12"].DefaultValue = cyr12.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr13"].DefaultValue = cyr13.Text.Trim();

                ObjectDataSource1.SelectParameters["prmlyr1"].DefaultValue = lyr1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr2"].DefaultValue = lyr2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr3"].DefaultValue = lyr3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr4"].DefaultValue = lyr4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr5"].DefaultValue = lyr5.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr6"].DefaultValue = lyr6.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr7"].DefaultValue = lyr7.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr8"].DefaultValue = lyr8.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr9"].DefaultValue = lyr9.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr10"].DefaultValue = lyr10.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr11"].DefaultValue = lyr11.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr12"].DefaultValue = lyr12.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr13"].DefaultValue = lyr13.Text.Trim();

                ObjectDataSource1.SelectParameters["prmbud1"].DefaultValue = bud1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud2"].DefaultValue = bud2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud3"].DefaultValue = bud3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud4"].DefaultValue = bud4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud5"].DefaultValue = bud5.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud6"].DefaultValue = bud6.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud7"].DefaultValue = bud7.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud8"].DefaultValue = bud8.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud9"].DefaultValue = bud9.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud10"].DefaultValue = bud10.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud11"].DefaultValue = bud11.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud12"].DefaultValue = bud12.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud13"].DefaultValue = bud13.Text.Trim();

                ObjectDataSource1.SelectParameters["prmly_bud1"].DefaultValue = ly_bud1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud2"].DefaultValue = ly_bud2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud3"].DefaultValue = ly_bud3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud4"].DefaultValue = ly_bud4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud5"].DefaultValue = ly_bud5.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud6"].DefaultValue = ly_bud6.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud7"].DefaultValue = ly_bud7.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud8"].DefaultValue = ly_bud8.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud9"].DefaultValue = ly_bud9.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud10"].DefaultValue = ly_bud10.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud11"].DefaultValue = ly_bud11.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud12"].DefaultValue = ly_bud12.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud13"].DefaultValue = ly_bud13.Text.Trim();                
                ObjectDataSource1.SelectParameters["prmtb_not_disc"].DefaultValue = Convert.ToString(trm.Checked);
                FormView1.ChangeMode(FormViewMode.ReadOnly);
            //}
            //catch
            //{
            //    return;
            //}
            //finally
            //{
               
            //}
        //}
        //else
        //{
        //}

    }

    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        TextBox curopn = (TextBox)FormView1.FindControl("cry_opnTextBox");
        TextBox lstopn = (TextBox)FormView1.FindControl("lyr_opnTextBox");
        TextBox desc = (TextBox)FormView1.FindControl("actdscrTextBox");
        Label cyr1 = (Label)FormView1.FindControl("cyr1TextBox");
        Label cyr2 = (Label)FormView1.FindControl("cyr2TextBox");
        Label cyr3 = (Label)FormView1.FindControl("cyr3TextBox");
        Label cyr4 = (Label)FormView1.FindControl("cyr4TextBox");
        Label cyr5 = (Label)FormView1.FindControl("cyr5TextBox");
        Label cyr6 = (Label)FormView1.FindControl("cyr6TextBox");
        Label cyr7 = (Label)FormView1.FindControl("cyr7TextBox");
        Label cyr8 = (Label)FormView1.FindControl("cyr8TextBox");
        Label cyr9 = (Label)FormView1.FindControl("cyr9TextBox");
        Label cyr10 = (Label)FormView1.FindControl("cyr10TextBox");
        Label cyr11 = (Label)FormView1.FindControl("cyr11TextBox");
        Label cyr12 = (Label)FormView1.FindControl("cyr12TextBox");
        Label cyr13 = (Label)FormView1.FindControl("cyr13TextBox");

        Label lyr1 = (Label)FormView1.FindControl("lyr1TextBox");
        Label lyr2 = (Label)FormView1.FindControl("lyr2TextBox");
        Label lyr3 = (Label)FormView1.FindControl("lyr3TextBox");
        Label lyr4 = (Label)FormView1.FindControl("lyr4TextBox");
        Label lyr5 = (Label)FormView1.FindControl("lyr5TextBox");
        Label lyr6 = (Label)FormView1.FindControl("lyr6TextBox");
        Label lyr7 = (Label)FormView1.FindControl("lyr7TextBox");
        Label lyr8 = (Label)FormView1.FindControl("lyr8TextBox");
        Label lyr9 = (Label)FormView1.FindControl("lyr9TextBox");
        Label lyr10 = (Label)FormView1.FindControl("lyr10TextBox");
        Label lyr11 = (Label)FormView1.FindControl("lyr11TextBox");
        Label lyr12 = (Label)FormView1.FindControl("lyr12TextBox");
        Label lyr13 = (Label)FormView1.FindControl("lyr13TextBox");

        TextBox bud1 = (TextBox)FormView1.FindControl("bud1TextBox");
        TextBox bud2 = (TextBox)FormView1.FindControl("bud2TextBox");
        TextBox bud3 = (TextBox)FormView1.FindControl("bud3TextBox");
        TextBox bud4 = (TextBox)FormView1.FindControl("bud4TextBox");
        TextBox bud5 = (TextBox)FormView1.FindControl("bud5TextBox");
        TextBox bud6 = (TextBox)FormView1.FindControl("bud6TextBox");
        TextBox bud7 = (TextBox)FormView1.FindControl("bud7TextBox");
        TextBox bud8 = (TextBox)FormView1.FindControl("bud8TextBox");
        TextBox bud9 = (TextBox)FormView1.FindControl("bud9TextBox");
        TextBox bud10 = (TextBox)FormView1.FindControl("bud10TextBox");
        TextBox bud11 = (TextBox)FormView1.FindControl("bud11TextBox");
        TextBox bud12 = (TextBox)FormView1.FindControl("bud12TextBox");
        TextBox bud13 = (TextBox)FormView1.FindControl("bud13TextBox");

        TextBox ly_bud1 = (TextBox)FormView1.FindControl("ly_bud1TextBox");
        TextBox ly_bud2 = (TextBox)FormView1.FindControl("ly_bud2TextBox");
        TextBox ly_bud3 = (TextBox)FormView1.FindControl("ly_bud3TextBox");
        TextBox ly_bud4 = (TextBox)FormView1.FindControl("ly_bud4TextBox");
        TextBox ly_bud5 = (TextBox)FormView1.FindControl("ly_bud5TextBox");
        TextBox ly_bud6 = (TextBox)FormView1.FindControl("ly_bud6TextBox");
        TextBox ly_bud7 = (TextBox)FormView1.FindControl("ly_bud7TextBox");
        TextBox ly_bud8 = (TextBox)FormView1.FindControl("ly_bud8TextBox");
        TextBox ly_bud9 = (TextBox)FormView1.FindControl("ly_bud9TextBox");
        TextBox ly_bud10 = (TextBox)FormView1.FindControl("ly_bud10TextBox");
        TextBox ly_bud11 = (TextBox)FormView1.FindControl("ly_bud11TextBox");
        TextBox ly_bud12 = (TextBox)FormView1.FindControl("ly_bud12TextBox");
        TextBox ly_bud13 = (TextBox)FormView1.FindControl("ly_bud13TextBox");

        RadioButtonList type = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
        CheckBox trm = (CheckBox)FormView1.FindControl("CheckBox2");

        UserClass UserLogin = (UserClass)Session["User"];

        //contact_list c1 = new contact_list();
        //bool check = c1.validatecustomer1(Convert.ToString(Session["Customers_Company"]), Convert.ToString(Session["User"]), "Validate", state.Text, zip.Text, city.Text, type.Text, desctype.Text, sman.Text, descsman.Text, terms.Text, descterms.Text, currcode.Text, loc.Text, descloc.Text, carrier.Text, descarrier.Text, delzone.Text, deszone.Text, terr.Text, desterr.Text, taxgr.Text);

        //string chec = Convert.ToString(check);

        //if (chec == "True")
        //{

        //    try
        //    {

                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmactdscr"].DefaultValue = desc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmactype"].DefaultValue = type.SelectedValue;

                ObjectDataSource1.SelectParameters["prmcry_opn"].DefaultValue = curopn.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr_opn"].DefaultValue = lstopn.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr1"].DefaultValue = cyr1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr2"].DefaultValue = cyr2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr3"].DefaultValue = cyr3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr4"].DefaultValue = cyr4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr5"].DefaultValue = cyr5.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr6"].DefaultValue = cyr6.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr7"].DefaultValue = cyr7.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr8"].DefaultValue = cyr8.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr9"].DefaultValue = cyr9.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr10"].DefaultValue = cyr10.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr11"].DefaultValue = cyr11.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr12"].DefaultValue = cyr12.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcyr13"].DefaultValue = cyr13.Text.Trim();

                ObjectDataSource1.SelectParameters["prmlyr1"].DefaultValue = lyr1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr2"].DefaultValue = lyr2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr3"].DefaultValue = lyr3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr4"].DefaultValue = lyr4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr5"].DefaultValue = lyr5.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr6"].DefaultValue = lyr6.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr7"].DefaultValue = lyr7.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr8"].DefaultValue = lyr8.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr9"].DefaultValue = lyr9.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr10"].DefaultValue = lyr10.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr11"].DefaultValue = lyr11.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr12"].DefaultValue = lyr12.Text.Trim();
                ObjectDataSource1.SelectParameters["prmlyr13"].DefaultValue = lyr13.Text.Trim();

                ObjectDataSource1.SelectParameters["prmbud1"].DefaultValue = bud1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud2"].DefaultValue = bud2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud3"].DefaultValue = bud3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud4"].DefaultValue = bud4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud5"].DefaultValue = bud5.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud6"].DefaultValue = bud6.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud7"].DefaultValue = bud7.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud8"].DefaultValue = bud8.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud9"].DefaultValue = bud9.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud10"].DefaultValue = bud10.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud11"].DefaultValue = bud11.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud12"].DefaultValue = bud12.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbud13"].DefaultValue = bud13.Text.Trim();

                ObjectDataSource1.SelectParameters["prmly_bud1"].DefaultValue = ly_bud1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud2"].DefaultValue = ly_bud2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud3"].DefaultValue = ly_bud3.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud4"].DefaultValue = ly_bud4.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud5"].DefaultValue = ly_bud5.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud6"].DefaultValue = ly_bud6.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud7"].DefaultValue = ly_bud7.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud8"].DefaultValue = ly_bud8.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud9"].DefaultValue = ly_bud9.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud10"].DefaultValue = ly_bud10.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud11"].DefaultValue = ly_bud11.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud12"].DefaultValue = ly_bud12.Text.Trim();
                ObjectDataSource1.SelectParameters["prmly_bud13"].DefaultValue = ly_bud13.Text.Trim();                
                ObjectDataSource1.SelectParameters["prmtb_not_disc"].DefaultValue = Convert.ToString(trm.Checked);
                FormView1.ChangeMode(FormViewMode.ReadOnly);
            //}
            //catch
            //{
            //    return;
            //}
            //finally
            //{
                //Response.Write("<script> window.location.href='view_gl_account.aspx' </script>");
        //    }
        //}
        //else
        //{
        //}




    }


    protected void delete_Button_Click(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        Session["customer1_list_index"] = null;
        /* Response.Write("<script>window.location.href = 'customer_list.aspx';</script>");*/
    }
    protected void copy_balance_bdgt(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "btn";
        Session["customer1_list_index"] = null;
        /* Response.Write("<script>window.location.href = 'customer_list.aspx';</script>");*/
    }
    protected void lnk_listglaccount(object sender, EventArgs e)
    {
        Response.Redirect("gl_account_list.aspx");
    }
    protected void lnk_viewglaccount_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_gl_account.aspx");
    }
    protected void load_viewglaccount_Click(object sender, EventArgs e)
    {
        Response.Redirect("distribute_account.aspx");
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


}