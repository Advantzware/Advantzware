#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;

#endregion

public partial class view_contact : System.Web.UI.Page
{
    private string Rec = "";
    protected void Page_Load(object sender, System.EventArgs e)
    {
        //FormView1.Visible = true;
        //FormView2.Visible = false;
        //FormView1.ChangeMode(FormViewMode.ReadOnly);


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_contacts.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            if (aUsers == "external")
            {

                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
            Session["view_contact_usr_login"] = vUserId;
            Session["view_contact_usr_comp"] = PrmComp;
        }
        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {

                lblUser.Text = UserLogin.UserName;

            }
            if (Session["contact_rec_key"] == null)
            {
                AddNewButton.Visible = true;
            }
            else
            {
                AddNewButton.Visible = false;
            }
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

    protected void contactSqlDataSource_Inserted(object sender, SqlDataSourceStatusEventArgs e)
    {
        if (e.Exception == null)
        {
            //lblMessage.Text = "<b>" + "Record inserted" + "</b><p>";
        }
        else
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }

    }
    protected void contactSqlDataSource_Updated(object sender, SqlDataSourceStatusEventArgs e)
    {
        if (e.Exception == null)
        {
            //lblMessage.Text = "<b>" + "Record inserted" + "</b><p>";
        }
        else
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }

    }
    protected void contactSqlDataSource_Inserting(object sender, SqlDataSourceCommandEventArgs e)
    {

    }
    protected void SqlDataSource1_Deleted(object sender, SqlDataSourceStatusEventArgs e)
    {
        if (e.Exception == null)
        {
            //lblMessage.Text = "<b>" + "Record inserted" + "</b><p>";
        }
        else
        {
            lblMessage.Text += "Error description" + ": " + e.Exception.Message + "<p>";
            e.ExceptionHandled = true;
        }
        //dbGrid_contact.SelectedIndex = -1;
    }


    protected void CopyButton_Click(object sender, EventArgs e)
    {
        FormView1.Visible = false;
        FormView2.Visible = true;
        FormView2.ChangeMode(FormViewMode.Insert);
        Label reckey = (Label)FormView1.FindControl("rec_keyLabel");
        Rec = reckey.Text;

    }
    protected void AddnewButton_click(object sender, EventArgs e)
    {
        Label reckey = (Label)FormView1.FindControl("rec_keyLabel");
        Rec = reckey.Text;
    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        try
        {
            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                TextBox reckey = (TextBox)FormView1.FindControl("TextBox3");
                TextBox rec_key = (TextBox)FormView1.FindControl("TextBox4");
                reckey.Text = DateTime.Today.ToString("MMddyyyy") + Convert.ToString(Convert.ToUInt64(Rec) + 1);
                Session["contact_reckey"] = reckey.Text;
                rec_key.Text = Convert.ToString(Session["contact_reckey"]);
                TextBox first_nameTextBox = (TextBox)FormView1.FindControl("first_nameTextBox");
                first_nameTextBox.Focus();
            }

            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                TextBox first_nameTextBox2 = (TextBox)FormView1.FindControl("first_nameTextBox");
                first_nameTextBox2.Focus();
            }
        }
        catch { Response.Write("Error"); }
        //try
        //{
        //    if (FormView1.CurrentMode == FormViewMode.Edit)
        //    {
        //        TextBox suppcode = (TextBox)FormView1.FindControl("comp_codeTextBox");
        //        Session["SupcodeTextBox"] = suppcode.Text;
        //    }
        //}
        //catch
        //{
        //    Response.Write("Error");
        //}
    }
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }


    protected void FormView2_DataBound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            TextBox custno = (TextBox)FormView2.FindControl("cust_noTextBox");
            TextBox shipid = (TextBox)FormView2.FindControl("ship_idTextBox");
            TextBox sman = (TextBox)FormView2.FindControl("smanTextBox");
            TextBox firstname = (TextBox)FormView2.FindControl("first_nameTextBox");
            TextBox lastname = (TextBox)FormView2.FindControl("last_nameTextBox");
            TextBox middlename = (TextBox)FormView2.FindControl("middle_initialTextBox");
            DropDownList sirname = (DropDownList)FormView2.FindControl("DropDownList2");
            TextBox contacttitle = (TextBox)FormView2.FindControl("contact_titleTextBox");
            CheckBox maillist = (CheckBox)FormView2.FindControl("maillistTextBox");
            TextBox type = (TextBox)FormView2.FindControl("typeTextBox");
            DropDownList contactloc = (DropDownList)FormView2.FindControl("SITextBox");
            TextBox custname = (TextBox)FormView2.FindControl("cust_nameTextBox");


            TextBox address1 = (TextBox)FormView2.FindControl("addr1TextBox");
            TextBox address2 = (TextBox)FormView2.FindControl("addr2TextBox");
            TextBox city = (TextBox)FormView2.FindControl("cityTextBox");
            TextBox state = (TextBox)FormView2.FindControl("stateTextBox");
            TextBox zip = (TextBox)FormView2.FindControl("zipTextBox");
            TextBox country = (TextBox)FormView2.FindControl("countryTextBox");
            TextBox county = (TextBox)FormView2.FindControl("countyTextBox");
            TextBox territority = (TextBox)FormView2.FindControl("territoryTextBox");
            TextBox accesscode = (TextBox)FormView2.FindControl("access_codeTextBox");
            TextBox phone = (TextBox)FormView2.FindControl("phoneTextBox");
            TextBox cellphone = (TextBox)FormView2.FindControl("cell_phoneTextBox");
            TextBox fax = (TextBox)FormView2.FindControl("faxTextBox");
            TextBox extension = (TextBox)FormView2.FindControl("extensionTextBox");
            TextBox email = (TextBox)FormView2.FindControl("emailTextBox");
            TextBox website = (TextBox)FormView2.FindControl("websiteTextBox");
            TextBox compcode = (TextBox)FormView2.FindControl("comp_codeTextBox");
            TextBox statuscode = (TextBox)FormView2.FindControl("status_codeTextBox");
            TextBox siccode = (TextBox)FormView2.FindControl("sic_codeTextBox");
            TextBox comp_des = (TextBox)FormView2.FindControl("comp_descTextBox");
            TextBox stat_des = (TextBox)FormView2.FindControl("status_descTextBox");
            TextBox sic_des = (TextBox)FormView2.FindControl("sic_descTextBox");
            TextBox first_nameTextBox = (TextBox)FormView2.FindControl("first_nameTextBox");
            first_nameTextBox.Focus();
            try
            {
                TextBox reckey = (TextBox)FormView2.FindControl("TextBox3");
                TextBox rec_key = (TextBox)FormView2.FindControl("TextBox4");
                reckey.Text = DateTime.Today.ToString("MMddyyyy") + Convert.ToString(Convert.ToUInt64(Rec) + 1);
                Session["contact_reckey"] = reckey.Text;
                rec_key.Text = Convert.ToString(Session["contact_reckey"]);
            }
            catch { return; }

            custno.Text = Convert.ToString(Session["contact_list_cust_no"]);
            shipid.Text = Convert.ToString(Session["contact_list_ship_id"]);
            middlename.Text = Convert.ToString(Session["contact_list_middle_initial"]);
            sman.Text = Convert.ToString(Session["contact_list_sman"]);
            firstname.Text = Convert.ToString(Session["contact_list_first_name"]);
            lastname.Text = Convert.ToString(Session["contact_list_last_name"]);
            sirname.SelectedValue = Convert.ToString(Session["contact_list_sirname"]);
            contacttitle.Text = Convert.ToString(Session["contact_list_contact_title"]);
            maillist.Checked = Convert.ToBoolean(Session["contact_list_maillist"]);
            type.Text = Convert.ToString(Session["contact_list_type"]);
            contactloc.SelectedValue = Convert.ToString(Session["contact_list_contact_loc"]);
            custname.Text = Convert.ToString(Session["contact_list_cust_name"]);
            address1.Text = Convert.ToString(Session["contact_list_addr1"]);
            address2.Text = Convert.ToString(Session["contact_list_addr2"]);
            city.Text = Convert.ToString(Session["contact_list_city"]);
            state.Text = Convert.ToString(Session["contact_list_state"]);
            zip.Text = Convert.ToString(Session["contact_list_zip"]);
            country.Text = Convert.ToString(Session["contact_list_country"]);
            county.Text = Convert.ToString(Session["contact_list_county"]);

            territority.Text = Convert.ToString(Session["contact_list_territory"]);
            accesscode.Text = Convert.ToString(Session["contact_list_access_code"]);
            phone.Text = Convert.ToString(Session["contact_list_phone"]);
            cellphone.Text = Convert.ToString(Session["contact_list_cell_phone"]);
            fax.Text = Convert.ToString(Session["contact_list_fax"]);
            extension.Text = Convert.ToString(Session["contact_list_extension"]);

            email.Text = Convert.ToString(Session["contact_list_email"]);
            website.Text = Convert.ToString(Session["contact_list_website"]);
            compcode.Text = Convert.ToString(Session["contact_list_comp_code"]);
            statuscode.Text = Convert.ToString(Session["contact_list_status_code"]);
            siccode.Text = Convert.ToString(Session["contact_list_sic_code"]);

            comp_des.Text = Convert.ToString(Session["contact_comp_des"]);
            stat_des.Text = Convert.ToString(Session["contact_status_des"]);
            sic_des.Text = Convert.ToString(Session["contact_sic_des"]);


            if (shipid.Text == "&nbsp;")
            {
                shipid.Text = "";
            }

            if (firstname.Text == "&nbsp;")
            {
                firstname.Text = "";
            }
            if (lastname.Text == "&nbsp;")
            {
                lastname.Text = "";
            }

            if (address1.Text == "&nbsp;")
            {
                address1.Text = "";
            }
            if (address2.Text == "&nbsp;")
            {
                address2.Text = "";
            }

            if (accesscode.Text == "&nbsp;")
            {
                accesscode.Text = "";
            }
            if (phone.Text == "&nbsp;")
            {
                phone.Text = "";
            }
            if (extension.Text == "&nbsp;")
            {
                extension.Text = "";
            }
            if (cellphone.Text == "&nbsp;")
            {
                cellphone.Text = "";
            }
            if (fax.Text == "&nbsp;")
            {
                fax.Text = "";
            }
            if (custname.Text == "&nbsp;")
            {
                custname.Text = "";
            }
            if (website.Text == "&nbsp;")
            {
                website.Text = "";
            }
            if (email.Text == "&nbsp;")
            {
                email.Text = "";
            }
            if (country.Text == "&nbsp;")
            {
                country.Text = "";
            }
            if (county.Text == "&nbsp;")
            {
                county.Text = "";
            }

        }
    }
    protected void lnk_listcontact_click(object sender, EventArgs e)
    {
        Response.Redirect("contact_list.aspx");
    }
    protected void lnk_notes_click(object sender, EventArgs e)
    {
        Response.Redirect("list_notes.aspx");
    }
    protected void supp_textchanged(object sender, EventArgs e)
    {
        TextBox supcode = (TextBox)FormView1.FindControl("comp_codeTextBox");
        Session["SupcodeTextBox"] = supcode.Text;
    }
    protected void supp2_textchanged(object sender, EventArgs e)
    {
        TextBox supcode2 = (TextBox)FormView2.FindControl("comp_codeTextBox");
        Session["SupcodeTextBox2"] = supcode2.Text;
    }
    protected void sic_textchanged(object sender, EventArgs e)
    {
        TextBox siccode = (TextBox)FormView1.FindControl("sic_codeTextBox");
        Session["SiccodeTextBox"] = siccode.Text;
    }
    protected void sic2_textchanged(object sender, EventArgs e)
    {
        TextBox siccode2 = (TextBox)FormView2.FindControl("sic_codeTextBox");
        Session["SiccodeTextBox2"] = siccode2.Text;
    }
    protected void status_textchanged(object sender, EventArgs e)
    {
        TextBox statusccode = (TextBox)FormView1.FindControl("status_codeTextBox");
        Session["StatuscodeTextBox"] = statusccode.Text;
    }
    protected void status2_textchanged(object sender, EventArgs e)
    {
        TextBox statusccode2 = (TextBox)FormView2.FindControl("status_codeTextBox");
        Session["StatuscodeTextBox2"] = statusccode2.Text;
    }

    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        TextBox first = (TextBox)FormView1.FindControl("first_nameTextBox");
        TextBox middle = (TextBox)FormView1.FindControl("middle_initialTextBox");
        TextBox last = (TextBox)FormView1.FindControl("last_nameTextBox");
        TextBox customercode = (TextBox)FormView1.FindControl("cust_noTextBox");
        TextBox company = (TextBox)FormView1.FindControl("cust_nameTextBox");
        TextBox address1 = (TextBox)FormView1.FindControl("addr1TextBox");
        TextBox address2 = (TextBox)FormView1.FindControl("addr2TextBox");
        TextBox city = (TextBox)FormView1.FindControl("cityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("zipTextBox");
        TextBox country = (TextBox)FormView1.FindControl("countryTextBox");
        TextBox county = (TextBox)FormView1.FindControl("countyTextBox");
        TextBox territory = (TextBox)FormView1.FindControl("territoryTextBox");
        TextBox title = (TextBox)FormView1.FindControl("contact_titleTextBox");
        TextBox salesman = (TextBox)FormView1.FindControl("TextBox2");
        TextBox shipid = (TextBox)FormView1.FindControl("ship_idTextBox");
        TextBox type = (TextBox)FormView1.FindControl("typeTextBox");
        TextBox accesscode = (TextBox)FormView1.FindControl("access_codeTextBox");
        TextBox phone = (TextBox)FormView1.FindControl("phoneTextBox");
        TextBox extension = (TextBox)FormView1.FindControl("extensionTextBox");
        TextBox cellphone = (TextBox)FormView1.FindControl("cell_phoneTextBox");
        TextBox website = (TextBox)FormView1.FindControl("websiteTextBox");
        TextBox email = (TextBox)FormView1.FindControl("emailTextBox");
        TextBox fax = (TextBox)FormView1.FindControl("faxTextBox");
        TextBox suppcode = (TextBox)FormView1.FindControl("comp_codeTextBox");
        TextBox siccode = (TextBox)FormView1.FindControl("sic_codeTextBox");
        TextBox statuscode = (TextBox)FormView1.FindControl("status_codeTextBox");
        CheckBox mail = (CheckBox)FormView1.FindControl("maillistTextBox");
        DropDownList sirname = (DropDownList)FormView1.FindControl("DropDownList1");
        DropDownList location = (DropDownList)FormView1.FindControl("SITextBox");
        TextBox compdesc = (TextBox)FormView1.FindControl("comp_descTextBox");
        TextBox sicdesc = (TextBox)FormView1.FindControl("sic_descTextBox");
        TextBox statusdesc = (TextBox)FormView1.FindControl("status_descTextBox");


        if (Page.IsValid)
        {
            contact_list c1 = new contact_list();
            bool check = c1.validatecontact2(Convert.ToString(Session["view_contact_usr_comp"]), Convert.ToString(Session["view_contact_usr_login"]), "Validate", title.Text, customercode.Text, company.Text, address1.Text, city.Text, state.Text, territory.Text, salesman.Text, type.Text, zip.Text);
            string chec = Convert.ToString(check);

            if (chec == "True")
            {
                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                try
                {
                    conn.Open();
                    SqlCommand cmd = new SqlCommand("update contact set sirname='" + sirname.Text + "',cust_no='" + customercode.Text + "', first_name='" + first.Text + "', middle_initial='" + middle.Text + "', last_name='" + last.Text + "', contact_title='" + title.Text + "', ship_id='" + shipid.Text + "', contact_loc='" + location.Text + "', type='" + type.Text + "', sman='" + salesman.Text + "', maillist='" + mail.Checked + "', access_code='" + accesscode.Text + "', phone='" + phone.Text + "', extension='" + extension.Text + "', cell_phone='" + cellphone.Text + "', fax='" + fax.Text + "', email='" + email.Text + "', website='" + website.Text + "', cust_name='" + company.Text + "', addr1='" + address1.Text + "', addr2='" + address2.Text + "', city='" + city.Text + "', state='" + state.Text + "', zip='" + zip.Text + "', county='" + country.Text + "', country='" + country.Text + "', territory='" + territory.Text + "', comp_code='" + suppcode.Text + "', status_code='" + statuscode.Text + "', sic_code='" + siccode.Text + "',status_des='" + statusdesc.Text + "',comp_des='" + compdesc.Text + "',sic_des='" + sicdesc.Text + "' where rec_key='" + Session["contact_rec_key"] + "'", conn);
                    cmd.ExecuteNonQuery();
                    conn.Close();
                }
                catch
                {
                    return;
                }
                finally
                {
                    conn.Close();
                }
                FormView1.ChangeMode(FormViewMode.ReadOnly);
            }
            else
            {
                //Response.Write("error");
            }
        }
        else
        {
            FormView1.ChangeMode(FormViewMode.Edit);
        }

    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        TextBox first = (TextBox)FormView1.FindControl("first_nameTextBox");
        TextBox middle = (TextBox)FormView1.FindControl("middle_initialTextBox");
        TextBox last = (TextBox)FormView1.FindControl("last_nameTextBox");
        TextBox customercode = (TextBox)FormView1.FindControl("cust_noTextBox");
        TextBox company = (TextBox)FormView1.FindControl("cust_nameTextBox");
        TextBox address1 = (TextBox)FormView1.FindControl("addr1TextBox");
        TextBox address2 = (TextBox)FormView1.FindControl("addr2TextBox");
        TextBox city = (TextBox)FormView1.FindControl("cityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("zipTextBox");
        TextBox country = (TextBox)FormView1.FindControl("countryTextBox");
        TextBox county = (TextBox)FormView1.FindControl("countyTextBox");
        TextBox territory = (TextBox)FormView1.FindControl("territoryTextBox");
        TextBox title = (TextBox)FormView1.FindControl("contact_titleTextBox");
        TextBox salesman = (TextBox)FormView1.FindControl("TextBox2");
        TextBox shipid = (TextBox)FormView1.FindControl("ship_idTextBox");
        TextBox type = (TextBox)FormView1.FindControl("typeTextBox");
        TextBox accesscode = (TextBox)FormView1.FindControl("access_codeTextBox");
        TextBox phone = (TextBox)FormView1.FindControl("phoneTextBox");
        TextBox extension = (TextBox)FormView1.FindControl("extensionTextBox");
        TextBox cellphone = (TextBox)FormView1.FindControl("cell_phoneTextBox");
        TextBox website = (TextBox)FormView1.FindControl("websiteTextBox");
        TextBox email = (TextBox)FormView1.FindControl("emailTextBox");
        TextBox fax = (TextBox)FormView1.FindControl("faxTextBox");
        TextBox suppcode = (TextBox)FormView1.FindControl("comp_codeTextBox");
        TextBox siccode = (TextBox)FormView1.FindControl("sic_codeTextBox");
        TextBox statuscode = (TextBox)FormView1.FindControl("status_codeTextBox");
        CheckBox mail = (CheckBox)FormView1.FindControl("maillistTextBox");
        DropDownList sirname = (DropDownList)FormView1.FindControl("DropDownList2");
        DropDownList location = (DropDownList)FormView1.FindControl("SITextBox");
        TextBox compdesc = (TextBox)FormView1.FindControl("comp_descTextBox");
        TextBox sicdesc = (TextBox)FormView1.FindControl("sic_descTextBox");
        TextBox statusdesc = (TextBox)FormView1.FindControl("status_descTextBox");

        TextBox rec_key = (TextBox)FormView1.FindControl("TextBox4");
        TextBox rec_key_value = (TextBox)FormView1.FindControl("TextBox3");


        if (Session["view_contact_usr_comp"] == null)
        {
            Session["view_contact_usr_comp"] = 1;
        }
        if (Page.IsValid)
        {
            contact_list c1 = new contact_list();
            bool check = c1.validatecontact2(Convert.ToString(Session["view_contact_usr_comp"]), Convert.ToString(Session["view_contact_usr_login"]), "Validate", title.Text, customercode.Text, company.Text, address1.Text, city.Text, state.Text, territory.Text, salesman.Text, type.Text, zip.Text);
            string chec = Convert.ToString(check);
            //Response.Write(check);




            if (chec == "True")
            {
                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                try
                {
                    conn.Open();
                    SqlCommand cmd = new SqlCommand("insert into contact (company,sirname, first_name, middle_initial, last_name, contact_title, cust_no, ship_id, contact_loc, type, sman, maillist, access_code, phone, extension, cell_phone, fax, email, website, cust_name, addr1, addr2, city, state, zip, county, country, territory, rec_key, comp_code, status_code, sic_code,comp_des,status_des,sic_des) values('" + Session["view_contact_usr_comp"] + "','" + sirname.Text + "','" + first.Text + "','" + middle.Text + "','" + last.Text + "','" + title.Text + "','" + customercode.Text + "','" + shipid.Text + "','" + location.Text + "','" + type.Text + "','" + salesman.Text + "','" + mail.Checked + "','" + accesscode.Text + "','" + phone.Text + "','" + extension.Text + "','" + cellphone.Text + "','" + fax.Text + "','" + email.Text + "','" + website.Text + "','" + company.Text + "','" + address1.Text + "','" + address2.Text + "','" + city.Text + "','" + state.Text + "','" + zip.Text + "','" + county.Text + "','" + country.Text + "','" + territory.Text + "','" + rec_key.Text + "','" + suppcode.Text + "','" + statuscode.Text + "','" + siccode.Text + "','" + compdesc.Text + "','" + statusdesc.Text + "','" + sicdesc.Text + "')", conn);
                    SqlCommand cmd2 = new SqlCommand("insert into contact_reckey (rec_key_value) values ('" + rec_key_value.Text + "')", conn);

                    cmd.ExecuteNonQuery();
                    cmd2.ExecuteNonQuery();
                    conn.Close();
                }
                catch
                {
                    return;
                }
                finally
                {
                    conn.Close();
                }
                //FormView1.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='contact_list.aspx'</script>");
            }
            else
            {

            }
        }
        else
        {
            FormView1.ChangeMode(FormViewMode.Insert);
            // Response.Write("<script>history.back()</script>");
        }

    }

    protected void InsertButton2_Click(object sender, EventArgs e)
    {
        TextBox custno = (TextBox)FormView2.FindControl("cust_noTextBox");
        TextBox shipid = (TextBox)FormView2.FindControl("ship_idTextBox");
        TextBox salesman = (TextBox)FormView2.FindControl("smanTextBox");
        TextBox first = (TextBox)FormView2.FindControl("first_nameTextBox");
        TextBox last = (TextBox)FormView2.FindControl("last_nameTextBox");
        TextBox middle = (TextBox)FormView2.FindControl("middle_initialTextBox");
        DropDownList sirname = (DropDownList)FormView2.FindControl("DropDownList2");
        TextBox title = (TextBox)FormView2.FindControl("contact_titleTextBox");
        CheckBox mail = (CheckBox)FormView2.FindControl("maillistTextBox");
        TextBox type = (TextBox)FormView2.FindControl("typeTextBox");
        DropDownList location = (DropDownList)FormView2.FindControl("SITextBox");
        TextBox company = (TextBox)FormView2.FindControl("cust_nameTextBox");


        TextBox address1 = (TextBox)FormView2.FindControl("addr1TextBox");
        TextBox address2 = (TextBox)FormView2.FindControl("addr2TextBox");
        TextBox city = (TextBox)FormView2.FindControl("cityTextBox");
        TextBox state = (TextBox)FormView2.FindControl("stateTextBox");
        TextBox zip = (TextBox)FormView2.FindControl("zipTextBox");
        TextBox country = (TextBox)FormView2.FindControl("countryTextBox");
        TextBox county = (TextBox)FormView2.FindControl("countyTextBox");
        TextBox territory = (TextBox)FormView2.FindControl("territoryTextBox");
        TextBox accesscode = (TextBox)FormView2.FindControl("access_codeTextBox");
        TextBox phone = (TextBox)FormView2.FindControl("phoneTextBox");
        TextBox cellphone = (TextBox)FormView2.FindControl("cell_phoneTextBox");
        TextBox fax = (TextBox)FormView2.FindControl("faxTextBox");
        TextBox extension = (TextBox)FormView2.FindControl("extensionTextBox");
        TextBox email = (TextBox)FormView2.FindControl("emailTextBox");
        TextBox website = (TextBox)FormView2.FindControl("websiteTextBox");
        TextBox suppcode = (TextBox)FormView2.FindControl("comp_codeTextBox");
        TextBox statuscode = (TextBox)FormView2.FindControl("status_codeTextBox");
        TextBox siccode = (TextBox)FormView2.FindControl("sic_codeTextBox");

        TextBox compdesc = (TextBox)FormView2.FindControl("comp_descTextBox");
        TextBox sicdesc = (TextBox)FormView2.FindControl("sic_descTextBox");
        TextBox statusdesc = (TextBox)FormView2.FindControl("status_descTextBox");



        TextBox customercode = (TextBox)FormView2.FindControl("cust_noTextBox");
        TextBox rec_key = (TextBox)FormView2.FindControl("TextBox4");
        TextBox rec_key_value = (TextBox)FormView2.FindControl("TextBox3");
        //Response.Write(rec_key_value.Text);

        if (Session["view_contact_usr_comp"] == null)
        {
            Session["view_contact_usr_comp"] = 1;
        }

        if (Page.IsValid)
        {
            contact_list c1 = new contact_list();
            bool check = c1.validatecontact2(Convert.ToString(Session["view_contact_usr_comp"]), Convert.ToString(Session["view_contact_usr_login"]), "Validate", title.Text, customercode.Text, company.Text, address1.Text, city.Text, state.Text, territory.Text, salesman.Text, type.Text, zip.Text);
            string chec = Convert.ToString(check);

            if (chec == "True")
            {
                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                try
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand("insert into contact (company,sirname, first_name, middle_initial, last_name, contact_title, cust_no, ship_id, contact_loc, type, sman, maillist, access_code, phone, extension, cell_phone, fax, email, website, cust_name, addr1, addr2, city, state, zip, county, country, territory, rec_key, comp_code, status_code, sic_code,comp_des,status_des,sic_des) values('" + Session["view_contact_usr_comp"] + "','" + sirname.Text + "','" + first.Text + "','" + middle.Text + "','" + last.Text + "','" + title.Text + "','" + customercode.Text + "','" + shipid.Text + "','" + location.Text + "','" + type.Text + "','" + salesman.Text + "','" + mail.Checked + "','" + accesscode.Text + "','" + phone.Text + "','" + extension.Text + "','" + cellphone.Text + "','" + fax.Text + "','" + email.Text + "','" + website.Text + "','" + company.Text + "','" + address1.Text + "','" + address2.Text + "','" + city.Text + "','" + state.Text + "','" + zip.Text + "','" + county.Text + "','" + country.Text + "','" + territory.Text + "','" + rec_key.Text + "','" + suppcode.Text + "','" + statuscode.Text + "','" + siccode.Text + "','" + compdesc.Text + "','" + statusdesc.Text + "','" + sicdesc.Text + "')", conn);
                    SqlCommand cmd2 = new SqlCommand("insert into contact_reckey (rec_key_value) values ('" + rec_key_value.Text + "')", conn);

                    cmd.ExecuteNonQuery();
                    cmd2.ExecuteNonQuery();
                    conn.Close();
                }
                catch
                {
                    return;
                }
                finally
                {
                    conn.Close();
                }

                Response.Write("<script>window.location.href='contact_list.aspx'</script>");
            }
            else
            {

            }
        }

        else
        {
            FormView1.Visible = false;
            FormView2.ChangeMode(FormViewMode.Insert);
        }

    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        {
            try
            {
                SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                conn.Open();
                SqlCommand cmd = new SqlCommand("delete from [dbo].[contact] where rec_key='" + Session["contact_rec_key"] + "'", conn);
                cmd.ExecuteNonQuery();
                conn.Close();
            }
            catch
            {
                return;
            }
            Response.Redirect("contact_list.aspx");
        }
    }
    protected void AddNewButton_Click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
        AddNewButton.Visible = false;
    }

    protected void Comp_Supp_Validate(object source, ServerValidateEventArgs args)
    {
        args.IsValid = ValidateSupCodeExists() ? true : false;
    }
    protected bool ValidateSupCodeExists()
    {
        string suppcode = Convert.ToString(Session["SupcodeTextBox"]);
        string returnValue = getRowValue();

        if (suppcode == returnValue)
            return true;

        else
            return false;

    }
    public string getRowValue()
    {
        string value = "";

        string suppcode = Convert.ToString(Session["SupcodeTextBox"]);
        string sqlLookup = "SELECT comp_code FROM comp_suppliers WHERE comp_code = '" + suppcode + "' ";
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        using (SqlCommand comm = new SqlCommand(sqlLookup, conn))
        {
            if (comm.Connection.State == ConnectionState.Closed)
            {
                comm.Connection.Open();
            }
            value = comm.ExecuteScalar() + "";
            comm.Connection.Close();
        }
        return value;

    }

    protected void Comp_Supp2_Validate(object source, ServerValidateEventArgs args)
    {
        args.IsValid = ValidateSup2CodeExists() ? true : false;
    }
    protected bool ValidateSup2CodeExists()
    {
        string suppcode = Convert.ToString(Session["SupcodeTextBox2"]);
        string returnValue = get2RowValue();

        if (suppcode == returnValue)
            return true;

        else
            return false;

    }
    public string get2RowValue()
    {
        string value = "";

        string suppcode = Convert.ToString(Session["SupcodeTextBox2"]);
        string sqlLookup = "SELECT comp_code FROM comp_suppliers WHERE comp_code = '" + suppcode + "' ";
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        using (SqlCommand comm = new SqlCommand(sqlLookup, conn))
        {
            if (comm.Connection.State == ConnectionState.Closed)
            {
                comm.Connection.Open();
            }
            value = comm.ExecuteScalar() + "";
            comm.Connection.Close();
        }
        return value;

    }

    protected void Comp_Sic_Validate(object source, ServerValidateEventArgs args)
    {
        args.IsValid = ValidateSicCodeExists() ? true : false;
    }
    protected bool ValidateSicCodeExists()
    {

        string siccode = Convert.ToString(Session["SiccodeTextBox"]);
        string returnvalue2 = getRowValue2();

        if (siccode == returnvalue2)
            return true;

        else
            return false;

    }
    public string getRowValue2()
    {
        string value2 = "";

        string siccode = Convert.ToString(Session["SiccodeTextBox"]);
        string sqlLookup = "SELECT industry_sic_code FROM industry_sic WHERE industry_sic_code = '" + siccode + "' ";
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        using (SqlCommand comm = new SqlCommand(sqlLookup, conn))
        {
            if (comm.Connection.State == ConnectionState.Closed)
            {
                comm.Connection.Open();
            }
            value2 = comm.ExecuteScalar() + "";
            comm.Connection.Close();
        }
        return value2;

    }

    protected void Comp_Sic2_Validate(object source, ServerValidateEventArgs args)
    {
        args.IsValid = ValidateSic2CodeExists() ? true : false;
    }
    protected bool ValidateSic2CodeExists()
    {

        string siccode = Convert.ToString(Session["SiccodeTextBox2"]);
        string returnvalue2 = get2RowValue2();

        if (siccode == returnvalue2)
            return true;

        else
            return false;

    }
    public string get2RowValue2()
    {
        string value2 = "";

        string siccode = Convert.ToString(Session["SiccodeTextBox2"]);
        string sqlLookup = "SELECT industry_sic_code FROM industry_sic WHERE industry_sic_code = '" + siccode + "' ";
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        using (SqlCommand comm = new SqlCommand(sqlLookup, conn))
        {
            if (comm.Connection.State == ConnectionState.Closed)
            {
                comm.Connection.Open();
            }
            value2 = comm.ExecuteScalar() + "";
            comm.Connection.Close();
        }
        return value2;

    }

    protected void Comp_Status_Validate(object source, ServerValidateEventArgs args)
    {
        args.IsValid = ValidateStatusCodeExists() ? true : false;
    }
    protected bool ValidateStatusCodeExists()
    {

        string statuscode = Convert.ToString(Session["StatuscodeTextBox"]);
        string returnvalue2 = getRowValue3();

        if (statuscode == returnvalue2)
            return true;

        else
            return false;

    }
    public string getRowValue3()
    {
        string value2 = "";

        string statuscode = Convert.ToString(Session["StatuscodeTextBox"]);
        string sqlLookup = "SELECT status_code FROM status WHERE status_code = '" + statuscode + "' ";
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        using (SqlCommand comm = new SqlCommand(sqlLookup, conn))
        {
            if (comm.Connection.State == ConnectionState.Closed)
            {
                comm.Connection.Open();
            }
            value2 = comm.ExecuteScalar() + "";
            comm.Connection.Close();
        }
        return value2;

    }

    protected void Comp_Status2_Validate(object source, ServerValidateEventArgs args)
    {
        args.IsValid = ValidateStatus2CodeExists() ? true : false;
    }
    protected bool ValidateStatus2CodeExists()
    {

        string statuscode = Convert.ToString(Session["StatuscodeTextBox2"]);
        string returnvalue2 = get2RowValue3();

        if (statuscode == returnvalue2)
            return true;

        else
            return false;

    }
    public string get2RowValue3()
    {
        string value2 = "";

        string statuscode = Convert.ToString(Session["StatuscodeTextBox2"]);
        string sqlLookup = "SELECT status_code FROM status WHERE status_code = '" + statuscode + "' ";
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        using (SqlCommand comm = new SqlCommand(sqlLookup, conn))
        {
            if (comm.Connection.State == ConnectionState.Closed)
            {
                comm.Connection.Open();
            }
            value2 = comm.ExecuteScalar() + "";
            comm.Connection.Close();
        }
        return value2;

    }
    protected void InsertCancelButton_Click23(object sender, EventArgs e)
    {
        FormView1.Visible = true;
        FormView2.Visible = false;
    }
    protected void lnk_MailList_click(object sender, EventArgs e)
    {
        Response.Redirect("contact_maillist.aspx");
    }
    protected void lnk_calendar_click(object sender, EventArgs e)
    {
        Response.Redirect("appointment.aspx");
    }
}
